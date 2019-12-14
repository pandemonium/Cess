#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FParsecCS.dll"
#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FParsec.dll"

#load "Syntax.fs"
#load "Parser.fs"

open Cess
open FParsec

run Parser.statement ("""
  for (;;) printf("hi, mom"); 
""".Trim ())