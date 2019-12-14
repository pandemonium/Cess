// Learn more about F# at http://fsharp.org

open System
open FParsec
open Cess

[<EntryPoint>]
let main argv =
  let expr = argv.[0]

  run Parser.expression expr
  |> printfn "%A"

  0 // return an integer exit code
