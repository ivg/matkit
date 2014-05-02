open Core.Std
open Ast

type t = script with sexp,compare


let load chan : t =
  Lexing.from_channel chan |> Parser.script Lexer.tokens

let save (script : t) chan : unit =
  let open Printer in
  let dot = string "." ++ flush in
  let pprs = List.map script ~f:(function
      | None, decs  -> Decl.ppr_list decs
      | Some _,decs -> let decs = Decl.ppr_list decs in
        string "unimplemented," ++ flush ++ string "where" ++
        decs ++ dot) in
  let ppr = list 0.0 flush pprs in
  let out = Format.formatter_of_out_channel chan in
  Printer.format ident out ppr;
  flush_all ()
