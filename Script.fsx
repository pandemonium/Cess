#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FParsecCS.dll"
#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FParsec.dll"
#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FSharp.Quotations.Evaluator.dll"

#load "Syntax.fs"
#load "Parser.fs"

open Cess
open FParsec
open FSharp.Quotations
open FSharp.Quotations.Evaluator


QuotationEvaluator.Evaluate <@ printer "%s %d" "h" 1 @>
