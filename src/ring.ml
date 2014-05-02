open Core.Std
open Ast

module T = struct
  type t = ring with sexp,compare,enumerate
end
include T

let real = R
let integer = Z
let complex = C

let to_string = function
  | Z -> "Z"
  | R -> "R"
  | C -> "C"

let expected =
  List.intersperse (List.map all ~f:to_string) ~sep:" | " |>
  String.concat

let of_string= function
  | "Z" -> Z
  | "C" -> C
  | "R" -> R
  | other ->
    let msg =
      sprintf "unrecongnized ring: %s, expecting [ %s ]"
        other expected in
    invalid_arg msg

let ppr t = to_string t |> Printer.string

include Comparable.Make(T)
