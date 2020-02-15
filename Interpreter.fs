namespace Cess

open AbstractSyntax

(* Maybe these are in fact Automat and not Continuation? *)
type Automaton =
  | Break    of Domain
  | Continue of Domain Environment

module Automaton =
  let bind f = function
    | Continue environment -> f environment
    | break'               -> break'

  let term orElse = function
    | Continue _ -> orElse
    | Break term -> term

module Interpreter =
  let expectedAbstraction<'a> : Symbol -> 'a =
    failwith << sprintf "%A - expected abstraction"

  let expectedSymbol<'a> : Symbol -> 'a =
    failwith << sprintf "%A - expected symbol"

  let tryLookupAbstraction name =
    Environment.tryLookup name 
      >> Option.bind Domain.abstraction

  let tryLookupValue name =
    Environment.tryLookup name 
      >> Option.bind Domain.value
    
  let truthy = function
    | Value (Int x) -> 
      x <> 0
    | x ->
      printfn "truthy: incomplete match of %A" x
      false

  let rec interpret environment = function
    | Ignore expression ->
      evaluate environment expression 
      |> (snd >> Continue)

    | If (predicate, consequent, alternative) ->
      let condition, environment' = evaluate environment predicate

      if truthy condition
        then consequent
        else alternative
      |> interpretBlock environment'

    | While (invariant, loopBody) ->
      let rec loop env =
        let condition, env' = evaluate env invariant

        if truthy condition
               (* Fekk: this is not(?) tail-recursive. *)
          then Automaton.bind loop <| interpretBlock env' loopBody
          else Continue env'

      loop <| Environment.enter environment

    | For (inits, invariants, updates, loopBody) ->
      let _, initialEnvironment = List.mapFold evaluate environment inits
      let rec loop env =
        let invariants', env' = List.mapFold evaluate env invariants

        if List.forall truthy invariants'
          then
            interpretBlock env' loopBody
            |> Automaton.bind (fun env'' ->
              let _, env''' = List.mapFold evaluate env'' updates

              loop env'''
            )
          else Continue env'
    
      loop <| Environment.enter initialEnvironment

    | Return expression ->
      let term, environment' = evaluate environment expression
      Break term

    (* Bind a random value to binding/name unless expression. *)
    | Declaration ((name, ty), expression) ->
      let term, 
          environment' = Option.mapFold evaluate environment expression
      let term'        = Option.defaultWith (fun _ -> Domain.defaultValue ty) term

      Environment.extend name term' environment'
      |> Continue

  and interpretBlock environment = function
    | Simple stmt ->
      interpret environment stmt
    | Compound stmts ->
      // Wtf!
      List.fold (fun c s -> 
                  Automaton.bind (fun e -> interpret e s) c
                )
                (Continue environment) 
                stmts

  (* Move this to some sort of resolveIntrinsic that can host this complexity. *)
  and applyIntrinsic symbol arguments environment =
    match symbol with
    | Name "+" ->
      let terms, environment' = List.mapFold evaluate environment arguments

      Intrinsic.plus terms, environment'
    | Name "<" ->
      let terms, environment' = List.mapFold evaluate environment arguments

      Intrinsic.lessThan terms, environment'
    | Name "printf" ->
      let terms, environment' = List.mapFold evaluate environment arguments

      Intrinsic.printf terms, environment'
    | name ->
      sprintf "Intrinsic `%A` not defined." name
      |> failwith

  and evaluate environment = function
    | Literal constant -> 
      Value constant, environment

    | Variable (Select name)
    | Variable (SelectIntrinsic name) ->
      Environment.tryLookup name environment
      |> Option.defaultWith (fun _ -> expectedSymbol name), 
      environment 

    | Apply (SelectIntrinsic name, arguments) ->
      applyIntrinsic name arguments environment

    | Apply (Select (Name "printf"), arguments) ->
      applyIntrinsic (Name "printf") arguments environment

    | Apply (Select name, arguments) -> 
      let _, _, formals, body = 
        tryLookupAbstraction name environment
        |> Option.defaultWith (fun _ -> expectedAbstraction name)

      let environment' = reduce arguments formals environment

      (* Error condition: void return with a declaration that
         promises a real value. *)
      interpretBlock environment' body
      |> Automaton.term (Value Void), environment'

    | Let (name, expression) ->
      let result, environment' = evaluate environment expression
      let konst _              = result

      result,
      Environment.update name konst environment'

  (* Will type-check actuals against formals at some point. *)
  and reduce actuals formals environment =
    let terms, environment' = List.mapFold evaluate environment actuals
    let names               = List.map fst formals
    let symbols             = List.zip names terms

    environment'
    |> Environment.enter
    |> Environment.withList symbols

  let start program = ()