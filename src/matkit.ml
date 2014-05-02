open Core.Std
open Ast
open Type

let infer_cmd =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-i" ~doc:"input filename" ~aliases:["--input" ]
      (optional_with_default "/dev/stdin" file)
    +> flag "-o" ~doc:"output filename" ~aliases:["--output" ]
      (optional_with_default "/dev/stdout" file)
  in
  Command.basic
    ~summary:"Checks and infers properties of the script"
    spec (fun input output () ->
        try
          let script = In_channel.with_file input ~f:Script.load in
          let script = Infer.process script in
          Out_channel.with_file output ~f:(Script.save script)
        with Type_error (d1,d2,subst) ->
          let err = Typing.ppr_error d1 d2 subst in
          let out = Format.std_formatter in
          Printer.format ident out err)


let command = Command.group ~summary:"Matkit is a Matrix Toolkit" [
    "infer", infer_cmd;
  ]

let () = Command.run ~version:"1.0" command
