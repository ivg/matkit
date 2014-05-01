open Core.Std
open Ast

module T = struct
  type t = dim with sexp,compare
  let hash = Hashtbl.hash
end
include T


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

let one = INum Nat1.one
let of_nat n = INum n
let of_int n = INum (Nat1.of_int_exn n)
let to_sym = to_string
let of_sym = of_string

let is_var = function
  | IVar _ -> true
  | INum _ | IConst _ -> false

let is_num = function
  | INum _ -> true
  | IVar _ | IConst _ -> false

let ppr d = d |> to_string |> Printer.string

type mapping = int -> t


module C = Comparable.Make(T)
module H = Hashable.Make(T)


let create_mapping ~const ~used : mapping =
  let create s =
    if const
    then IConst (Sym.capitalize s)
    else IVar   (Sym.uncapitalize s) in
  let is_used d = C.Set.mem used d in
  let is_free d = not (is_used d) in
  let dims =
    List.map [ "N"; "M"; "P"; "Q"; "K"; "L" ] ~f:create |>
    List.filter  ~f:is_free |>
    Array.of_list in
  fun (n : int) ->
    let sym =
      if  n < Array.length dims
      then dims.(n)
      else
        let nth n = sprintf "T_%d" (n-Array.length dims+1) in
        let rec search_free n =
          let var = create (nth n) in
          if is_free var then var else search_free (n+1) in
        search_free n in
    sym

include C
include H
