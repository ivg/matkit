open Core.Std
open Ast
open Type




let one = INum One
let scalar = (one, one)
let vector d1 = (d1, one)
let matrix d1 d2 = (d1,d2)

let equal_constr (l1,r1) (l2,r2)  = [l1,r1; l2, r2]

let type_of_string = function
  | "mat" | "matrix" | "matrices" -> `Matrix
  | "vec" | "vector" | "vectors"  -> `Vector
  | "num" | "sca" | "scalar" | "scalars" -> `Scalar
  | s -> `Unknown s

let is_upper s = Sym.exists s ~f:Char.is_uppercase
let is_lower s = Sym.exists s ~f:Char.is_lowercase

let generate_lexical_constraints env =
  let rec loop exp =
    match exp with
    | Num _ -> []
    | Uop (_,e1) | Sub (e1,_,_) -> loop e1
    | Bop (_,e1,e2) -> List.concat [loop e1; loop e2]
    | Var _ when Env.is_bound env exp -> []
    | Var s when is_lower s ->
      let (_,ri) = Env.get_or_add_fresh env exp in
      [ri, one]
    | Var _ -> [] in
  loop

let init exp cs =
  let env = Env.empty () in
  let cs = List.fold cs ~init:[] ~f:(fun constrs (sym,constr) ->
      let (li,ri) = Env.get_or_add_fresh env (Var (String.of_char sym)) in
      match type_of_string constr with
      | `Matrix -> constrs
      | `Vector -> (ri,one) :: constrs
      | `Scalar -> (ri,one) :: (li,one) :: constrs
      | `Unknown s ->
        printf "Warning> unknown property %s" s;
        constrs) in
  match exp with
  | Some exp ->
    env, List.concat [generate_lexical_constraints env exp; cs]
  | None -> env, cs

(** [binary_constr t1 t2 ty op] given an expression $A `op` B = C$,
    with [A:t1, B:t2, C:ty] generate constraint according to
    operation [op] *)
let binary_constr ((l1,r1) as t1) ((l2,r2) as t2) ((l3,r3) as ty) = function
  | Mul -> [l1,l3; r2,r3; r1,l2]
  | (Sub|Add|Had) -> List.concat [
      equal_constr t1 t2;
      equal_constr t1 ty;
      equal_constr t2 ty;
    ]
  | Pow -> List.concat [
      equal_constr t1 scalar;
      equal_constr t2 ty;
    ]

let unary_constr ((l1,r1) as t1) ((l2,r2) as t2) = function
  | Tran -> [ l1, r2; r1, l2 ]
  | (UNeg|Conj) -> equal_constr t1 t2

let rec recon ctx expr : (ty * constrs) =
  let (lt,rt) as tt = Env.get_or_add_fresh ctx expr in
  match expr with
  | Num _ ->  scalar ,[]
  | Var id -> tt,[]
  | Sub (s,i1,i2) ->
    let _,cs = recon ctx s in
    tt, List.concat [
      (match i1,i2 with
      | None, Some _ -> [rt,one]
      | Some _, None -> [lt,one]
      | Some _, Some _ -> [rt,one; lt,one]
      | None, None -> []);
      cs
    ]
  | Bop (op,s1,s2) ->
    let t1,c1 = recon ctx s1 in
    let t2,c2 = recon ctx s2 in
    tt, List.concat [binary_constr t1 t2 tt op; c1; c2]
  | Uop (op,s1) ->
    let t1,c1 = recon ctx s1 in
    tt,List.concat [unary_constr t1 tt op; c1]


module Subst = struct
  include Hashable.Make(struct
      type t = ty with sexp,compare
      let hash = Hashtbl.hash
    end)

  let extend subs (t1,t2) = ()
end

let infer env expr =
  let (ty,cs) = recon env expr in
  (* let rec unify cs subst = match cs with *)
  (*   | [] -> subst *)
  (*   | c::cs -> match c with *)
  (*     | (IVar v, rhs) -> (match Subst.find v with *)
  (*       | Some v -> unify (v,rhs :: cs) *)
  (*       | None -> ) *)
  (*   subst in *)
  ty
