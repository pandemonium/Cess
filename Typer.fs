namespace Cess

open AbstractSyntax


type TypeEnvironment = TypeTerm Environment

module TypeTerm =
  let rec name = function
    | TypeTerm.Select (Name name) -> name
    | Pointer typeTerm   -> sprintf "%s *" (name typeTerm)
    | Array typeTerm     -> sprintf "%s []" (name typeTerm)
    | Arrow (args, ret)  -> sprintf "%s%s" (tupleName args) (name ret)
  and tupleName types =
    List.map name types
    |> String.concat ", "
    |> sprintf "(%s)" 

  let arrow = function
    | Arrow (tuple, return') -> Some (tuple, return')
    | _                      -> None

module Typer =
  let todo<'a> : 'a = failwith "Not implemented yet!"

  let expectedArrow<'a> : Symbol -> 'a =
    failwith << sprintf "%A - expected function type"

  let expectedSymbol<'a> : Symbol -> 'a =
    failwith << sprintf "%A - expected symbol"

  let tryLookupArrow name =
    Environment.tryLookup name 
      >> Option.bind TypeTerm.arrow

  let rec infer environment = function
    | Variable (Select name)
    | Variable (SelectIntrinsic name) ->
      Environment.tryLookup name environment
      |> Option.defaultWith (fun _ -> expectedSymbol name)

    | Literal constant ->
      typeOfConstant constant

    | Apply (SelectIntrinsic name, arguments) ->
//      printfn "Apply-Intrinsic %s on (%A) with (%A)" (Symbols.name name) arguments environment
      let formals =
        arguments |> List.map (infer environment)

      List.last formals

    | Apply (Select name, arguments) ->
      printfn "Apply %s on (%A) with (%A)" (Symbols.name name) arguments environment
      let formals, returnType = 
        tryLookupArrow name environment
        |> Option.defaultWith (fun _ -> expectedArrow name)

      (formals, arguments)
      ||> List.zip
      |> List.iter ((<||) (expect environment))

      returnType

    | Let (_, expression) ->
      infer environment expression

  and expect environment expectedType expression = 
    let receivedType = infer environment expression
    if receivedType <> expectedType
      then 
        let expectedName = TypeTerm.name expectedType
        let receivedName = TypeTerm.name receivedType
        failwith <| sprintf "expected %s; received %s" expectedName receivedName
      else ()

  and typeOfConstant = function
    | Constant.Char  _ -> Intrinsic.Type.Char
    | Constant.Float _ -> Intrinsic.Type.Float
    | Constant.Int   _ -> Intrinsic.Type.Int
    | Constant.Text  _ -> Intrinsic.Type.Text
    | Constant.Void  _ -> Intrinsic.Type.Void

  let rec checkStatement environment expectedReturnType = function
    | Ignore expression ->
      infer environment expression |> ignore
      environment

    | If (predicate, consequent, alternative) ->
      expect environment Intrinsic.Type.Int predicate
      checkBlock environment expectedReturnType consequent
      |> ignore
      checkBlock environment expectedReturnType alternative

    | While (invariant, block) ->
      expect environment Intrinsic.Type.Int invariant
      checkBlock environment expectedReturnType block

    | For (inits, invariants, updates, block) ->
      inits      |> List.map (infer environment) |> ignore
      invariants |> List.iter (checkNot environment Intrinsic.Type.Void)
      updates    |> List.map (infer environment) |> ignore

      checkBlock environment expectedReturnType block

    | Return expression ->
      expect environment expectedReturnType expression
      environment

    | Declaration decl -> 
      checkVarDeclaration environment decl

  and checkNot environment unexpectedType expression =
    let receivedType = infer environment expression
    if infer environment expression = unexpectedType
      then 
        let unexpectedName = TypeTerm.name unexpectedType
        let receivedName   = TypeTerm.name receivedType
        failwith <| sprintf "expected not %s; received %s" unexpectedName receivedName
      else ()

  and checkBlock environment expectedReturnType = function
    | Simple statement ->
      checkStatement environment expectedReturnType statement
    | Compound statements ->
      statements
      |> List.fold (fun env stmt -> checkStatement env expectedReturnType stmt) environment

  and checkVarDeclaration environment ((name, type'), initializer) =
    initializer
    |> Option.iter (expect environment type')

    environment
    |> Environment.extend name type'

  let checkFunction environment = function
    | returnType, name, typedBindings, body ->
      let formals      = List.map snd typedBindings
      let functionSelf = Arrow (formals, returnType)
      let environment' = 
        environment
        |> Environment.extend name functionSelf
        
      let environment'' =
        environment'
        |> Environment.enter
        |> Environment.withList typedBindings

      checkBlock environment'' returnType body
      |> ignore

      environment'

  let checkVariable = 
    checkVarDeclaration

  let checkToplevel environment = function
    | ToplevelDeclaration.Function f -> 
      checkFunction environment f
    | ToplevelDeclaration.Variable v ->
      checkVariable environment v
    | ToplevelDeclaration.Type t     -> 
      todo

  (* This has to be a fold. *)
  let checkProgram environment (CompilationUnit decls) =
    decls
    |> List.fold checkToplevel environment