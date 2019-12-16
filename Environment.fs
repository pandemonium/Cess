namespace Cess

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

  let deriveOfList symbols =
    derive <| Map.ofList symbols

  let deriveNew =
    derive Map.empty

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