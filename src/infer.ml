open Core.Std
open Ast
open Type

(** extracts a list of variables from an expression. The result is
    sorted and duplicates are removed. *)
let bound_variables : exp option -> sym list = function
  | None -> []
  | Some exp ->
    let bound = Exp.fold exp ~init:[] ~f:(fun e acc ->
      match e with
      | Var e -> e :: acc
      | _ -> acc) in
    bound |>
    List.sort ~cmp:Sym.ascending |>
    List.remove_consecutive_duplicates ~equal:Sym.equal

(** removes rings from declarations  *)
let remove_rings: decls -> decls =
  List.filter ~f:(function (s,Ring _) -> false | _ -> true)

(** creates a ring maker for a var, based on a set of user
    declarations. In a case of multiple declarations the maximum ring
    is chosen, according to the following ordering: [Z < R < C].  *)
let ring_for_var decs var : Decl.ring_decl =
  let ds = List.filter_map decs (function
      | (v,Ring (ring,_)) when Sym.(v = var) -> Some ring
      | _ -> None) in
  let decl_of_ring = function
    | R -> Decl.real var
    | C -> Decl.complex var
    | Z -> Decl.integer var in
  match ds with
  | [] -> decl_of_ring R
  | r::rs -> decl_of_ring (List.fold rs ~init:r ~f:Ring.max)

let process script : script =
  let subst = Typing.infer script in
  let all_decs = List.map script ~f:(fun (_,ds) -> ds) |> List.concat in
  let create_ring = ring_for_var all_decs in
  let process_stmt ((exp,decs) : stmt) =
    let vars = bound_variables exp in
    let decs' = List.map vars ~f:(fun sym ->
        match Exp.Map.find_exn subst (Var sym) with
        | ty when Typing.is_scalar ty -> Decl.scalar sym
        | (d1,d2) -> create_ring sym ~d1 ~d2) in
    exp, remove_rings decs @ decs' in
  List.map script ~f:process_stmt
