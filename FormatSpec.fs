namespace Cess

  open Microsoft.FSharp.Quotations
  open Microsoft.FSharp.Core.Printf
  open AbstractSyntax


  type FormatSpec =
    | Flag    of char
    | Literal of string


  module FormatParser =
    open FParsec

    module Result =
      let ofParserResult = function
        | Success (a, b, c) -> Result.Ok a
        | Failure (a, b, c) -> Result.Error (a, c)

    let flagCode    = anyOf "scdf%"
    let beginFlag   = pchar '%'
    let literalChar = noneOf "%"

    let flag        = beginFlag >>. flagCode |>> Flag
    let literal     = many1Chars literalChar |>> Literal
    let spec        = flag <|> literal
    let formatSpec  = many spec .>> eof

    let parse = 
      run formatSpec >> Result.ofParserResult


  module Show =
    let term = function
      | Value (Text x)  -> sprintf "%s" x
      | Value (Int x)   -> sprintf "%d" x
      | Value (Char x)  -> sprintf "%c" x
      | Value (Float x) -> sprintf "%f" x
      | _               -> "Doh!"


  module Format =
    let todo<'a> : 'a = failwith "todo."

    let tryRenderFormatted (terms: Domain list) =
      let Value (Text formatSpec) :: arguments = terms

      let formatArgument (args, output) = function
        | Flag _ ->
          let term :: tail = args
          tail,
          sprintf "%s%s" output (Show.term term)
        | Literal c ->
          args,
          sprintf "%s%s" output c

      FormatParser.parse formatSpec
      |> Result.map (
        List.fold formatArgument (arguments, "")
      )
      |> Result.map snd
