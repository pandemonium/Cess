namespace Cess

  open System
  open AbstractSyntax

  module Debug =
    let todo<'a> : 'a = failwith "Not implemented!"

  module Option =
    let mapFold (f: 's -> 'a -> 'b * 's) (acc: 's) = function
      | Some a -> 
        let b, s = f acc a
        Some b, s
      | None   -> 
        None, acc

  type Term =
    | Abstraction of FunctionDecl
    | Value       of Constant

  module Term =
    let fold onAbstraction onValue = function
      | Abstraction decl -> onAbstraction decl
      | Value term       -> onValue term

    let abstraction = fold Some (fun _ -> None)
    let value       = fold (fun _ -> None) Some

    let defaultValue ty : Term = Debug.todo

  type Symbols = Map<Symbol, Term>

  module Symbols =
    let name (Name name) = name

  type Environment =
    | Nil
    | Frame of Symbols * Environment

  module Environment =
    let empty = Nil

    let derive symbols baseline =
      Frame (symbols, baseline)

    let deriveOfList symbols baseline =
      derive (Map.ofList symbols) baseline

    let rec tryResolve name = function
      | Nil -> 
        None
      | Frame (data, baseline) ->
        data
        |> Map.tryFind name
        |> Option.bind (fun _ -> tryResolve name baseline)

    let tryResolveAbstraction name =
      tryResolve name >> Option.bind Term.abstraction

    let tryResolveValue name =
      tryResolve name >> Option.bind Term.value

    let rec addBinding name value = function
      | Nil -> 
        addBinding name value <| deriveOfList [] Nil
      | Frame (data, baseline) ->
        Frame (Map.add name value data, baseline)

    let rec tryUpdateBinding name (f: Term -> Term) = function
      | Nil ->
        None

      | Frame (data, baseline) when Map.containsKey name data ->
        let current = Map.find name data
        let data'   = Map.add name (f current) data

        Some <| Frame (data', baseline)

      | Frame (data, baseline) ->
        tryUpdateBinding name f baseline
        |> Option.map (derive data)
      
  module Interpreter =
    let expectedAbstraction =
      failwith << sprintf "%A - expected abstraction"

    let expectedSymbol =
      failwith << sprintf "%A - expected symbol"

    let truthy term = Debug.todo

    (* A let-binding is very clearly a joint effort together with evaluate. *)
    let rec interpret environment = function
      | Ignore expression ->
        evaluate environment expression |> snd

      | If (predicate, whenTrue, whenFalse) ->
        let condition, environment' = evaluate environment predicate

        if truthy condition
          then whenTrue
          else whenFalse
        |> interpretBlock environment'

      | While (predicate, loopBody) ->

          (* Updates to values in the environment must be made at the
             level of their declaration.

             int i = 0, j = 0;

             // an add(i, i + 1) here would mean that
             // after the while, i would still be 0.
             while (j < 10) i = i + 1;
           *)

        let rec loop env =
          let condition, env' = evaluate env predicate

          if truthy condition
            then loop <| interpretBlock env' loopBody
            else env'

        loop environment

      | For (inits, predicates, updates, loopBody) ->
        environment

      | Return expression ->
        environment

      (* Bind a random value to binding/name unless expression. *)
      | Declaration ((ty, name), expression) ->
        let term, 
            environment' = Option.mapFold evaluate environment expression
        let term'        = Option.defaultWith (fun _ -> Term.defaultValue ty) term

        Environment.addBinding name term' environment'

    (* A block has a return value. Doesn't it? No! *)
    and interpretBlock environment = function
      | Simple stmt    -> interpret environment stmt
      | Compound stmts -> List.fold interpret environment stmts

    and applyIntrinsic symbol arguments =
      Debug.todo

    and evaluate environment = function
      | Literal constant -> 
        Value constant, environment

      | Variable (Select name)
      | Variable (SelectIntrinsic name) ->
        Environment.tryResolve name environment
        |> Option.defaultValue (expectedSymbol name), 
        environment

      | Apply (SelectIntrinsic name, arguments) ->
        applyIntrinsic name arguments, environment

      | Apply (Select name, arguments) -> 
        let _, _, formals, body = 
          Environment.tryResolveAbstraction name environment
          |> Option.defaultValue (expectedAbstraction name)

        let environment' = reduce arguments formals environment

        (* Evaluating the Return statement produces an expression
           that can be evaluated and continued. 

           Then what do I return after interpreting a procedure call?

           let returnExpression = 
         *)

        Value Void, interpretBlock environment' body

      | Let (name, expression) ->
        let result, environment' = evaluate environment expression
        let expected _           = failwith << sprintf "expected symbol: %A"
        let konst _              = result

        result, 
        Environment.tryUpdateBinding name konst environment'
        |> Option.defaultWith (expected name)

    (* Will type-check actuals against formals at some point. *)
    and reduce actuals formals environment =
      let terms, environment' = List.mapFold evaluate environment actuals
      let names               = List.map (fun (_, name) -> name) formals
      let symbols             = List.zip names terms

      Environment.deriveOfList symbols environment'

    let start = ()