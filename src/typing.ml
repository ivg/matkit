open Core.Std
open Debug
open Ast
open Type

let one = INum Nat1.one

(** generate variable type according to a constrained provied by a
    user *)
let type_of_string = function
  | "mat" | "matrix" | "matrices" -> `Matrix
  | "vec" | "vector" | "vectors"  -> `Vector
  | "num" | "sca" | "scalar" | "scalars" -> `Scalar
  | s -> `Unknown s

let is_lower s = Sym.exists s ~f:Char.is_lowercase

(** generate constraints according to lexical conventions.
    The conventions are applied to variables, that are not constrained
    explicitly by a user. The conventions are:
    Lowercase variables are column-vectors. Everything else is a matrix. *)
let lexical_constraints env script =
  let rec gen exp =
    match exp with
    | Num _ -> []
    | Uop (_,e1) | Ind (e1,_,_) -> gen e1
    | Bop (_,e1,e2) -> gen e1 @ gen e2
    | Var _ when Env.is_bound env exp -> []
    | Var s when is_lower s ->
      let (_,ri) = Env.get_or_add_fresh env exp in
      [ri, one]
    | Var _ -> [] in
  List.map script ~f:(function
      | Some exp,_ -> gen exp
      | None,_ -> []) |>
  List.concat

(** adds user's ring constraints to the typing environment  *)
let init_env (script : script) =
  let env = Env.empty () in
  List.iter script ~f:(fun (_exp,decls) ->
      List.iter decls ~f:(fun (sym,prop) ->
          match prop with
          | Kind _ | Ring (_,None) -> ()
          | Ring (_, Some ty) -> Env.add env (Var sym) ty));
  env

(** [init exp constraints] initialize an environment from the
    expression [exp] with respect to user constraints *)
let init (script : script) =
  let env = init_env script in
  let cs = List.map script ~f:(fun (_,decl) ->
      List.map decl ~f:(fun (sym,property) -> match property with
          | Ring _ -> []
          | Kind p ->
            let (li,ri) = Env.get_or_add_fresh env (Var sym) in
            match type_of_string p with
            | `Matrix -> []
            | `Vector -> [ri,one]
            | `Scalar -> [ri,one; li,one]
            | `Unknown s ->
              eprintf "Warning> unknown property %s\n" s;
              [])) |>
           List.concat in
  env, List.concat (lexical_constraints env script :: cs)

(** [binary_constr t1 t2 t3 op] given an expression $A `op` B = C$,
    with [A:t1, B:t2, C:t3] generate constraint according to
    operation [op] *)
let binary_constr (l1,r1) (l2,r2) (l3,r3) = function
  | Mul -> [l1,l3; r2,r3; r1,l2]
  | Eql|HDiv|HPow|Sub|Add|Had ->
    [l1,l2; l1,l3; l2,l3; r1,r2; r1,r3; r2,r3;]
  | Pow -> [l1,r1; l3,r3; l1,r1; l2,one; r2,one]
  | Div -> [l1,one; r1,one; l2,one; r2,one; l3,one; r3,one]

(** impose constraints on unary expression  *)
let unary_constr (l1,r1) (l2,r2) = function
  | Tran ->        [l1,r2; r1,l2]
  | (UNeg|Conj) -> [l1,l2; r1,r2]

(** [recon ctx expr] reconstruct a type of [expr], generating a set of
    constraints. *)
let rec recon ctx expr : (ty * constrs) =
  let (lt,rt) as tt = Env.get_or_add_fresh ctx expr in
  match expr with
  | Num _ ->  tt,[lt,one; rt,one]
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
  include Dim.Table

  let replace t s = map t ~f:(substitute s)

  let extend_replace t (t1,t2) =
    add_exn t ~key:t1 ~data:t2;
    replace t (t1,t2)
end

let type_error (d1,d2) = raise (Type_error (d1,d2))

let swap (a,b) = (b,a)

let rec unify cs subst = match cs with
  | [] -> subst
  | (x,y)::cs when x = y -> unify cs subst
  | ((IConst _, IConst _) as ty) :: _
  | ((IConst _, INum _)   as ty) :: _
  | ((INum _,   IConst _ )  as ty) :: _
  | ((INum _,   INum _)     as ty) :: _ -> type_error ty
  | ((IConst _ |INum _),IVar _) as c::cs -> unify (swap c :: cs) subst
  | (lhs,rhs)::cs -> match Subst.find subst lhs with
    | None -> unify cs (Subst.extend_replace subst (lhs,rhs))
    | Some v -> unify ((rhs,v) :: cs) subst

let infer (script : script) : subst =
  let env,ucs = init script in
  let cs = List.map script ~f:(function
      | Some exp,_ -> snd(recon env exp)
      | None,_ -> []) in
  let cs = List.concat (ucs :: cs) |>
           List.sort ~cmp:compare  |>
           List.dedup in
  let subst = Subst.create () in
  let subst = unify cs subst in
  let ds = Subst.fold subst ~init:(UnionFind.create ())
      ~f:(fun ~key:lhs ~data:rhs set -> UnionFind.union set lhs rhs) in
  Env.create_substitution env ds
