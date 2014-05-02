open Core.Std
open Ast

type t = script with sexp,compare


let load chan : t =
  Lexing.from_channel chan |> Matlan_parser.script Matlan_lexer.tokens

let save (script : t) chan : unit =
  let open Printer in
  let dot = string "." ++ flush in
  let pprs = List.map script ~f:(function
      | None, decs  -> Decl.ppr_list decs
      | Some exp,decs ->
        let decs = Decl.ppr_list decs in
        box 2 (Exp.ppr exp) ++ string "," ++
        flush ++ string "where" ++
        decs ++ dot) in
  let ppr = list 0.1 flush pprs in
  let out = Format.formatter_of_out_channel chan in
  Printer.format (fun ppr -> box 2 ppr) out ppr;
  flush_all ()
