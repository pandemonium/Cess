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

  let defaultValue (ty: TypeTerm) : Term = 
    match ty with
    | TypeTerm.Select (Name "int") -> 
      Int 0xDEADBEEF |> Value
    | _ -> 
      Debug.todo


type Symbols = Map<Symbol, Term>

module Symbols =
  let name (Name name) = name


type Environment =
  | Nil
  | Scope of Symbols * Environment

module Environment =
  let empty = Nil

  let makeChildScope symbols enclosingScope =
    Scope (symbols, enclosingScope)

  let makeChildScopeOfList =
    makeChildScope << Map.ofList

  let makeEmptyChildScope =
    makeChildScope Map.empty

  let rec tryLookup name = function
    | Nil -> 
      None
    | Scope (data, baseline) ->
      data
      |> Map.tryFind name
      |> Option.orElseWith (fun _ -> tryLookup name baseline)

  let tryLookupAbstraction name =
    tryLookup name >> Option.bind Term.abstraction

  let tryLookupValue name =
    tryLookup name >> Option.bind Term.value

  let rec extend name value = function
    | Nil -> 
      extend name value <| makeChildScopeOfList [] Nil
    | Scope (data, baseline) ->
      Scope (Map.add name value data, baseline)

  let rec tryUpdateBinding name (f: Term -> Term) = function
    | Nil ->
      None

    | Scope (data, baseline) when Map.containsKey name data ->
      let current = Map.find name data
      let data'   = Map.add name (f current) data

      Some <| Scope (data', baseline)

    | Scope (data, baseline) ->
      tryUpdateBinding name f baseline
      |> Option.map (makeChildScope data)