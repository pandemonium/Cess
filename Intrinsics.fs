namespace Cess

open AbstractSyntax
open System

module Constant =
  let typeName = function
    | Int _   -> "Int"
    | Char _  -> "Char"
    | Float _ -> "Float"
    | Text _  -> "Text"
    | Void _  -> "Void"

module Intrinsic =
  let plus = function
    | (Term.Value lhs) :: (Term.Value rhs) :: _ ->
      match lhs, rhs with
      | Int i, Int j     -> i + j |> Int
      | Char i, Char j   -> i + j |> Constant.Char
      | Float i, Float j -> i + j |> Float
      | _ ->
        sprintf "No %s -> %s -> <any> signature"
          (Constant.typeName lhs) (Constant.typeName rhs)
        |> failwith
    |> Term.Value
    | _ -> failwith "yeah, no."

  let lessThan = function
    | (Term.Value lhs) :: (Term.Value rhs) :: _ ->
      match lhs, rhs with
      | Int i, Int j     -> if i < j then 1 else 0
      | Char i, Char j   -> if i < j then 1 else 0
      | Float i, Float j -> if i < j then 1 else 0
      | _ ->
        sprintf "No %s -> %s -> <any> signature"
          (Constant.typeName lhs) (Constant.typeName rhs)
        |> failwith
    |> Constant.Int |> Term.Value
    | _ -> failwith "yeah, no."

  let printf terms =
    Format.tryRenderFormatted terms
    |> Result.map (printf "%s")
    |> ignore

    Int 0 |> Value

//  let printf (terms: Term list) =
//    let show = sprintf "%A"
//
//    let text =
//      terms
//      |> Seq.ofList
//      |> Seq.map show
//      |> String.concat ""
//
//    printfn "%s" text
//
//    Int 0 |> Value