open System
open FParsec
open Cess


[<EntryPoint>]
let main argv =
//  let expr = argv.[0]

  let expr = """
  {
    int i = 0;

    while (i < 5) {
      printf ("i: %d", i);
      i = i + 1;
    }
    
    printf ("after: %d", i);
  }
  """

  run Parser.block (expr.Trim ())
  |> printfn "%A"

  0 // return an integer exit code
