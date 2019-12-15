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

  type SymbolTable = Map<Symbol, Term>

  type Environment =
    | Empty
    | Branch of SymbolTable * Environment

  module Environment =
    let todo<'a> : 'a = failwith "Not implemented!"

    let empty = Empty

    let withParent symbols parent =
      Branch (Map.ofList symbols, parent)

    let symbolName (Name name) = name

    let rec resolve name = function
      | Empty -> 
        name 
        |> symbolName
        |> sprintf "unresolved symbol: %A" 
        |> failwith
      | Branch (data, parent) ->
        data
        |> Map.tryFind name
        |> Option.defaultWith (fun _ -> resolve name parent)

    let rec add name value = function
      | Empty -> 
        withParent [] Empty
        |> add name value
      | Branch (data, parent) ->
        Branch (Map.add name value data, parent)

  
  module Interpreter =
    let todo<'a> : 'a = failwith "Not implemented!"

    (* Refactor to make it possible to have an easy resolveValue. *)
    let resolveAbstraction environment name =
      let expectation =
        sprintf "expected function: %A" name
        |> failwith

      Environment.resolve name environment
      |> Term.fold id expectation

    (* A let-binding is very clearly a joint effort together with evaluate. *)
    let rec interpret environment = function
      | x -> todo

    (* *)
    and evaluate environment = function
      | Literal constant -> 
        constant

      | Variable name -> 
        Environment.resolve name environment

      | Apply (name, arguments) -> 
        let _, _, formals, body = resolveAbstraction name environment

        let environment' = reduce arguments formals environment
        // What do I return or do here?
        // What if the result of the function does not
        // return anything? What does that mean?
        interpret environment' body

      (* This wants to affect the environment; how does that propagate. *)
      (* Clearly this has to be something like a Statement Expression.  *)
      | Let (name, expression) ->
        evaluate environment expression
        |> Environment.add name environment

        todo

    (* Will type-check actuals against formals at some point. *)
    and reduce actuals formals environment =
      let names = formals |> List.map (fun (_, name) -> name)
      let symbols =
        actuals
        |> List.map (evaluate environment >> Value)
        |> List.zip names
      
      Environment.withParent symbols environment

    let start = ()