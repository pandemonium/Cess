namespace Cess

  open FParsec
  open Syntax


  module Parser =
    let todo<'a> : 'a = failwith "Not implemented!"


    let ws = spaces

    let normalChar = 
      satisfy (fun c -> c <> '\\' && c <> '"' && c <> ''')

    let unescape = function
      | 'n' -> '\n'
      | 'r' -> '\r'
      | 't' -> '\t'
      | c   -> c

    let escapedChar = 
      pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)

    let singleQuote = pstring "'"

    let doubleQuote = pstring "\""

    let enclosedWithin delimiter = 
      between delimiter delimiter

    let charLiteral =
      normalChar <|> escapedChar
      |> enclosedWithin singleQuote

    let stringLiteral =
      normalChar <|> escapedChar
      |> manyChars
      |> enclosedWithin doubleQuote

    let constant =
      (pint32        |>> Int)   <|>
      (charLiteral   |>> Char)  <|>
      (pfloat        |>> Float) <|>
      (stringLiteral |>> Text)

    let literal = constant .>> ws |>> Literal

    let identifier = 
      many1Chars (letter <|> pchar '_') .>> ws |>> Name

    let variable = identifier |>> (Variable << Select)

    let expression, expressionRef = createParserForwardedToRef ()

    let comma = pstring "," .>> ws

    let arguments = sepBy expression comma

    let openArgs = pstring "(" .>> ws

    let closeArgs = pstring ")" .>> ws

    let argumentList =
      between openArgs closeArgs arguments

    let functionName =
      identifier |>> Select

    let functionCall =
      tuple2 functionName argumentList |>> Apply

    let assign = pstring "=" .>> ws

    let letBinding = 
      tuple2 (identifier .>> assign) expression |>> Let

    let term =
      (attempt functionCall) <|> literal <|> variable 

    // Should this take care of `=` ?
    let mkApply op lhs rhs =
      Apply (Operator <| Name op, [ lhs; rhs ])

    (* Does not handle paranthesis yet. *)
    let operators =
      let opp = new OperatorPrecedenceParser<Expression, unit, unit> ()
      opp.TermParser <- term

      let infixOperator prefix precedence associativity =
        InfixOperator (prefix, ws, precedence, associativity, mkApply prefix)
        |> opp.AddOperator

      infixOperator "*" 20 Associativity.Left
      infixOperator "+" 10 Associativity.Left
      infixOperator "<" 5 Associativity.Left
      infixOperator ">" 5 Associativity.Left
      infixOperator "=" 1 Associativity.Left

      opp.ExpressionParser

    expressionRef := operators

    let statement, statementRef = createParserForwardedToRef ()

    let semicolon = pstring ";" .>> ws

    let keyword name = pstring name .>> ws

    let ignoredExpression =
      expression .>> semicolon |>> Ignore

    let simpleBlock = statement |>> Block.Simple

    let block = simpleBlock

    let predicateSection = between openArgs closeArgs expression

    let ifStatement =
      let whenFalse = keyword "else" >>. block
      let whenTrue  = block

      keyword "if" >>. tuple3 predicateSection whenTrue whenFalse |>> If

    let whileStatement =
      let loopBody = block
      keyword "while" >>. tuple2 predicateSection loopBody |>> While


    let forStatement =
      let letBindings = sepBy expression comma
      let conditions  = sepBy expression comma
      let updates     = sepBy expression comma
      let preamble    =
        tuple3
          (letBindings .>> semicolon) 
          (conditions .>> semicolon) 
          updates

      keyword "for"
          >>. between openArgs closeArgs preamble
          .>>. block
          |>> (fun ((a, b, c), d) -> a, b, c, d)
          |>> For

    let returnStatement =
      keyword "return" >>. expression .>> semicolon |>> Return

    let simpleType = identifier |>> TypeTerm.Select

    let typeTerm = simpleType

    let typedBinding = typeTerm .>>. identifier |>> Simple

    let declareStatement = 
      typedBinding  .>>. opt expression .>> semicolon |>> Declare 

    statementRef := 
      ifStatement     <|>
      whileStatement  <|>
      forStatement    <|>
      returnStatement <|>
      declareStatement <|>
      ignoredExpression
