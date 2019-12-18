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

  match run Parser.block (expr.Trim ()) with
  | Success (block, _, _) ->
    let env = Environment.empty
    let env' = Interpreter.interpretBlock env block

    printfn "%A" env'

  | Failure _ as fail ->
    printfn "%A" fail
  

  0 // return an integer exit code
