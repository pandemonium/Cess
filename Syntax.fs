namespace Cess

module AbstractSyntax =

  type Tree =
    | CompilationUnit of ToplevelDeclaration list

  and TypedBinding     = TypeTerm * Symbol

  and FunctionDecl     = TypeTerm * Symbol * TypedBinding list * Block

  and VariableDecl     = TypedBinding * Expression option

  and TypeDecl         = Symbol * TypedBinding list

  and ToplevelDeclaration =
    | Function        of FunctionDecl
    | Variable        of VariableDecl
    | Type            of TypeDecl

  and LetBinding       = Symbol * Expression

  and Expression =
    | Literal         of Constant
    | Variable        of ValueTerm
    | Apply           of ValueTerm * Expression list
    | Let             of LetBinding

  and Block =
    | Simple          of Statement
    | Compound        of Statement list

  and Expressions      = Expression list

  and Statement =
    | Ignore          of Expression
    | If              of Expression * Block * Block
    | While           of Expression * Block
    | For             of Expressions * Expressions * Expressions * Block
    | Return          of Expression
    | Declaration     of VariableDecl

  and TypeTerm =
    | Select          of Symbol
    | Pointer         of TypeTerm
    | Array           of TypeTerm

  and ValueTerm =
    | Select          of Symbol
    | SelectIntrinsic of Symbol

  and Constant =    
    | Int             of int
    | Char            of char
    | Float           of float
    | Text            of string
    | Void

  and Symbol =    
    | Name            of string
