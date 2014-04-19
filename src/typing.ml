open Core.Std
open Ast
open Type

let one = INum One

let type_of_string = function
  | "mat" | "matrix" | "matrices" -> `Matrix
  | "vec" | "vector" | "vectors"  -> `Vector
  | "num" | "sca" | "scalar" | "scalars" -> `Scalar
  | s -> `Unknown s

let is_lower s = Sym.exists s ~f:Char.is_lowercase

let generate_lexical_constraints env =
  let rec loop exp =
    match exp with
    | Num _ -> []
    | Uop (_,e1) | Ind (e1,_,_) -> loop e1
    | Bop (_,e1,e2) -> loop e1 @ loop e2
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

(** [binary_constr t1 t2 t3 op] given an expression $A `op` B = C$,
    with [A:t1, B:t2, C:t3] generate constraint according to
    operation [op] *)
let binary_constr (l1,r1) (l2,r2) (l3,r3) = function
  | Mul -> [l1,l3; r2,r3; r1,l2]
  | Sub|Add|Had -> [l1,l2; l1,l3; l2,l3; r1,r2; r1,r3; r2,r3;]
  | Pow -> [l1,one; l2,one; l2,l3; r2,r3]

let unary_constr (l1,r1) (l2,r2) = function
  | Tran ->        [l1,r2; r1,l2]
  | (UNeg|Conj) -> [l1,l2; r1,r2]

let rec recon ctx expr : (ty * constrs) =
  let (lt,rt) as tt = Env.get_or_add_fresh ctx expr in
  match expr with
  | Num _ ->  (one,one) ,[]
  | Var _ -> tt,[]
  | Ind (s,i1,i2) ->
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


let substitute (f,t) x = if x = f then t else x


module Subst = struct
  include Index.Table

  let replace t s = map t ~f:(substitute s)

  let extend_replace t (t1,t2) =
    add_exn t ~key:t1 ~data:t2;
    replace t (t1,t2)

  let to_string t =
    Sexp.to_string_hum (sexp_of_t sexp_of_index t)

end



exception Type_error of nat1 * nat1 with sexp

let is_var = function
  | IVar _ -> true
  | INum _ -> false

let rec unify cs subst = match cs with
  | [] -> subst
  | (INum n, INum m)::_ when n <> m -> raise (Type_error (n,m))
  | (x,y)::cs when x = y -> unify cs subst
  | (x,y)::cs when not (is_var x) -> unify ((y,x)::cs) subst
  | (lhs,rhs)::cs -> match Subst.find subst lhs with
    | None -> unify cs (Subst.extend_replace subst (lhs,rhs))
    | Some v -> unify ((rhs,v) :: cs) subst

let infer cs expr =
  let env,ucs = init (Some expr) cs in
  let (_, cs) = recon env expr in
  let cs = cs @ ucs |> List.sort ~cmp:compare |> List.dedup in
  let subst = Subst.create () in
  let subst = unify cs subst in
  let ds = Subst.fold subst ~init:(UnionFind.create ())
      ~f:(fun ~key:lhs ~data:rhs set -> UnionFind.union set lhs rhs) in
  Env.create_substitution env ds
  (* printf "%s\n s.t.\n%s\nenv = \n%s\nsubst = %s%!\n" *)
  (*   (Exp.to_string expr) *)
  (*   (Sexp.to_string_hum (sexp_of_constrs cs)) *)
  (*   (Sexp.to_string_hum (Env.sexp_of_t env)) *)
  (*   (Subst.to_string subst); *)
