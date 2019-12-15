namespace Cess

  open System
  open AbstractSyntax

  type Term =
    | Abstraction of FunctionDecl
    | Value       of Constant

  module Term =
    let fold onAbstraction onValue = function
      | Abstraction decl -> onAbstraction decl
      | Value term       -> onValue term

    let abstraction = fold Some (fun _ -> None)
    let value       = fold (fun _ -> None) Some

  type Symbols = Map<Symbol, Term>

  module Symbols =
    let name (Name name) = name

  type Environment =
    | Nil
    | Frame of Symbols * Environment

  module Environment =
    let todo<'a> : 'a = failwith "Not implemented!"

    let empty = Nil

    let derive symbols baseline =
      Frame (Map.ofList symbols, baseline)

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

    let rec add name value = function
      | Nil -> 
        add name value <| derive [] Nil
      | Frame (data, baseline) ->
        Frame (Map.add name value data, baseline)

  
  module Interpreter =
    let todo<'a> : 'a = failwith "Not implemented!"

    let expectedAbstraction =
      failwith << sprintf "%A - expected abstraction"

    let expectedSymbol =
      failwith << sprintf "%A - expected symbol"

    let truthy term = todo

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
      | Declaration (binding, expression) ->
        
      
        environment

    and interpretBlock environment = function
      | Simple stmt    -> interpret environment stmt
      | Compound stmts -> List.fold interpret environment stmts

    and applyIntrinsic symbol arguments =
      todo

    and evaluate environment = function
      | Literal constant -> 
        Value constant, environment

      (* name is a NameTerm; where does Operator tryResolve? *)
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

           What if I separate functions from procedures?

           Then what do I return after interpreting a procedure call?

           Evaluate to ()?
         *)
        Value Void, interpretBlock environment' body

      | Let (name, expression) ->
        let result, environment' = evaluate environment expression

        result, Environment.add name result environment'

    (* Will type-check actuals against formals at some point. *)
    and reduce actuals formals environment =
      let terms, environment' = List.mapFold evaluate environment actuals
      let names               = List.map (fun (_, name) -> name) formals
      let symbols             = List.zip names terms

      Environment.derive symbols environment'

    let start = ()