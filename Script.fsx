#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FParsecCS.dll"
#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FParsec.dll"

#load "Syntax.fs"
#load "Parser.fs"

open Cess
open FParsec

run Parser.block ("""
  {
  int c;
  int d = 10;

  for (c = 0; c < d; c = c + 1)
  {
    printf("hi, mom [%d]", c);
  }

  return d;
  }
""".Trim ())