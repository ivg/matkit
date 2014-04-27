open Core.Std
open Ast

module T = struct
  type t = dim with sexp,compare
  let hash = Hashtbl.hash
end
include T
include Comparable.Make(T)
include Hashable.Make(T)


let to_string = function
  | IVar v | IConst v -> v
  | INum n -> Nat1.to_string n

let of_char c =
  match Char.get_digit c with
  | Some n -> INum (Nat1.of_int_exn n)
  | None when Char.is_lowercase c -> IVar (Char.to_string c)
  | None -> IConst (Char.to_string c)

let of_string s = match Sym.to_list s with
  | [] -> invalid_arg "empty dim string"
  | [c] ->  of_char c
  | c :: '_' :: _ -> if Char.is_lowercase c then IVar s else IConst s
  | _ -> invalid_arg ("invalid dim format: " ^ s)
