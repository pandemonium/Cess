namespace Cess

module Syntax =

  type Program =
    | CompilationUnit of ToplevelDeclaration list

  and ToplevelDeclaration =
    | Function        of Symbol * TypedBinding list * TypeTerm * Statement
    | Type            of Symbol * TypedBinding list

  and Expression =
    | Literal         of Constant
    | Variable        of NameTerm
    | Apply           of NameTerm * Expression list
    | Let             of LetBinding

  and Block =
    | Simple          of Statement
    | Compound        of Statement list

  and LetBinding      = Symbol * Expression

  and Expressions     = Expression list

  and Statement =
    | Ignore          of Expression
    | If              of Expression * Block * Block
    | While           of Expression * Block
    | For             of Expressions * Expressions * Expressions * Block
    | Return          of Expression
    | Declare         of TypedBinding * Expression option

  and TypeTerm =
    | Select          of Symbol
    | Pointer         of TypeTerm
    | Array           of TypeTerm

  and NameTerm =
    | Select          of Symbol
    | Operator        of Symbol

  and TypedBinding =
    | Simple          of TypeTerm * Symbol

  and Constant =    
    | Int             of int
    | Char            of char
    | Float           of float
    | Text            of string

  and Symbol =    
    | Name            of string