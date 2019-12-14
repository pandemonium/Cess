namespace Cess

module Syntax =

  type Program =
    | CompilationUnit of ToplevelDeclaration list

  and FunctionDecl     = TypeTerm * Symbol * TypedBinding list * Block

  and ToplevelDeclaration =
    | Function        of FunctionDecl
    | Variable        of Declaration
    | Type            of Symbol * TypedBinding list

  and Expression =
    | Literal         of Constant
    | Variable        of NameTerm
    | Apply           of NameTerm * Expression list
    | Let             of LetBinding

  and Block =
    | Simple          of Statement
    | Compound        of Statement list

  and LetBinding       = Symbol * Expression

  and Expressions      = Expression list

  and Declaration      = TypedBinding * Expression option

  and Statement =
    | Ignore          of Expression
    | If              of Expression * Block * Block
    | While           of Expression * Block
    | For             of Expressions * Expressions * Expressions * Block
    | Return          of Expression
    | Declaration     of Declaration

  and TypeTerm =
    | Select          of Symbol
    | Pointer         of TypeTerm
    | Array           of TypeTerm

  and NameTerm =
    | Select          of Symbol
    | Operator        of Symbol

  and TypedBinding     = TypeTerm * Symbol

  and Constant =    
    | Int             of int
    | Char            of char
    | Float           of float
    | Text            of string

  and Symbol =    
    | Name            of string
