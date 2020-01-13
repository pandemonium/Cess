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
 

type Domain =
  | Abstraction of FunctionDecl
  | Value       of Constant

module Domain =
  let fold onAbstraction onValue = function
    | Abstraction decl -> onAbstraction decl
    | Value term       -> onValue term

  let abstraction = fold Some (fun _ -> None)
  let value       = fold (fun _ -> None) Some

  let defaultValue (ty: TypeTerm) : Domain = 
    match ty with
    | TypeTerm.Select (Name "int") -> 
      Int 0xDEADBEEF |> Value
    | _ -> 
      Debug.todo


type 't Symbols = Map<Symbol, 't>

module Symbols =
  let name (Name name) = name


type 't Environment =
  | Nil
  | Scope of 't Symbols * 't Environment

module Environment =
  let empty = Nil

  let enter enclosingScope =
    Scope (Map.empty, enclosingScope)

  let noSuchSymbol name =
    failwith <| sprintf "No such symbol %s" (Symbols.name name)

  let rec extend name value = function
    | Nil ->
      extend name value <| enter Nil
    | Scope (symbols, scope) ->
      Scope (Map.add name value symbols, scope)

  let withList symbols =
    List.foldBack <| (<||) extend <| symbols

  let rec update name f = function
    | Nil ->
      noSuchSymbol name
    | Scope (symbols, scope) ->
      match Map.tryFind name symbols with
      | Some value -> Scope (Map.add name (f value) symbols, scope)
      | None       -> Scope (symbols, update name f scope)

  let rec tryLookup name = function
    | Nil ->
      None
    | Scope (symbols, scope) ->
      Map.tryFind name symbols
      |> Option.orElseWith (fun _ -> tryLookup name scope)