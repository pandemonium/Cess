open System
open FParsec
open Cess


[<EntryPoint>]
let main argv =
//  let expr = argv.[0]

  let expr = """
  {
    int i;
    int j = 1;

    for (i = 0; i < 20000; i = i + 1) {
      j = j + 1;
    }

    printf ("after - i: %d, j: %d", i, j);
  }
  """

  match run Parser.block (expr.Trim ()) with
  | Success (block, _, _) ->
    let env = Environment.empty
    Interpreter.interpretBlock env block
    |> ignore

  | Failure _ as fail ->
    printfn "%A" fail
  

  0 // return an integer exit code
