open System
open FParsec
open Cess


let compilationUnit = """

  int fib (int x) {
    return fib(x + 1) + fib(x + 2);
  }

  int main ()
  {
    int i;
    int j = 1;

    for (i = 0; i < 5; i = i + 1) {
      printf ("i: %d; j: %d\n", i, j);
      j = j + 1;
    }

    printf ("after - i: %d, j: %d\n", i, j);
    
    return 0;
  }

"""

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

  match run Parser.compilationUnit (compilationUnit.Trim ()) with
  | Success (compilation, _, _) ->
    let env = Environment.empty
    Typer.checkProgram env compilation
//    Interpreter.interpretBlock env block
    |> ignore

  | Failure _ as fail ->
    printfn "%A" fail
  

  0 // return an integer exit code
