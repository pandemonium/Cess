#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FParsecCS.dll"
#r @"/Users/pa/Documents/Code/Hurril/Cess/bin/Debug/netcoreapp3.0/FParsec.dll"

#load "Syntax.fs"
#load "Parser.fs"

open Cess
open FParsec


//run Parser.compilationUnit """
//  int g = 10;
//  int main(char argv) {
//    int c;
//    printf("Hi, mom: %d %d", c, j);
//
//    for (c = 0; c < 10; c = c + 1)
//      puts ("hello, world!");
//
//    return 0;
//  }
//"""