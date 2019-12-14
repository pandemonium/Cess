namespace Cess

  open System
  open Syntax

  type ValueSpace =
    | Function of FunctionDecl
    | Value    of Constant

  module ValueSpace =
    let fold onFunction onValue = function
      | Function decl  -> onFunction decl
      | Value constant -> onValue constant

  type SymbolTable = Map<Symbol, ValueSpace>

  type Environment =
    | Root            of SymbolTable
    | LevelWithParent of SymbolTable * Environment

  module Environment =
    let todo<'a> : 'a = failwith "Not implemented!"

    let make = Root

    let withParent symbols parent =
      LevelWithParent (Map.ofList symbols, parent)

    let resolve name   = todo
    let add name value = todo

  module Interpreter =
    let todo<'a> : 'a = failwith "Not implemented!"

    let resolveFunction env name =
      let expectation =
        sprintf "expected function: %A" name
        |> failwith

      Environment.resolve name env
      |> ValueSpace.fold id expectation

    (* A let-binding is very clearly a joint effort together with reduce. *)
    let rec evaluate env = function
      | x -> todo

    and reduce environment = function
      | Literal constant -> 
        constant

      | Variable name -> 
        Environment.resolve name

      | Apply (name, actuals) -> 
        let _, _, formals, body = resolveFunction name environment

        let environment' = applyParameters actuals formals environment
        // What do I return or do here?
        // What if the result of the function does not
        // return anything? What does that mean?
        evaluate environment' body

      (* This wants to affect the environment; how does that propagate. *)
      (* Clearly this has to be something like a Statement Expression.  *)
      | Let (name, expression) ->
        reduce environment expression
        |> Environment.add name

    (* Will type-check actuals against formals at some point. *)
    and applyParameters actuals formals environment =
      let names = formals |> List.map (fun (_, name) -> name)
      let symbols =
        actuals
        |> List.map (reduce environment >> Value)
        |> List.zip names
      
      Environment.withParent symbols environment

    let start = ()