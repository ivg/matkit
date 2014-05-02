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
    | Num _ ->
      let (li,ri) = Ctx.get_or_add_fresh env exp in
      [li,one; ri,one]
    | Uop (_,e1) | Ind (e1,_,_) -> gen e1
    | Bop (_,e1,e2) -> gen e1 @ gen e2
    | Var _ when Ctx.is_bound env exp -> []
    | Var s when is_lower s ->
      let (_,ri) = Ctx.get_or_add_fresh env exp in
      [ri, one]
    | Var _ -> [] in
  List.map script ~f:(function
      | Some exp,_ -> gen exp
      | None,_ -> []) |>
  List.concat

(** adds user's ring constraints to the typing environment  *)
let user_constraints env (script : script) =
  List.map script ~f:(fun (_exp,decls) ->
      List.map decls ~f:(fun (sym,prop) ->
          match prop with
          | Kind _ | Ring (_,None) -> []
          | Ring (_, Some (t1,t2)) ->
            let (ld,rd) = Ctx.get_or_add_fresh env (Var sym) in
            [ld,t1; rd,t2]))
  |> List.concat |> List.concat

(** [init exp constraints] initialize an environment from the
    expression [exp] with respect to user constraints *)
let init (script : script) =
  let env = Ctx.empty () in
  let ucs = user_constraints env script in
  let cs = List.map script ~f:(fun (_,decl) ->
      List.map decl ~f:(fun (sym,property) -> match property with
          | Ring _ -> []
          | Kind p ->
            let (li,ri) = Ctx.get_or_add_fresh env (Var sym) in
            match type_of_string p with
            | `Matrix -> []
            | `Vector -> [ri,one]
            | `Scalar -> [ri,one; li,one]
            | `Unknown s ->
              eprintf "Warning> unknown property %s\n" s;
              [])) |>
           List.concat in
  env, List.concat (lexical_constraints env script :: ucs :: cs)

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

let scalar_constr s1 s2 (l3,r3) = match s1,s2 with
  | (_,true),(_,true) -> [l3,one; r3,one]
  | (_,true),((l1,r1),false)
  | ((l1,r1),false),(_,true) ->  [l1,l3; r1,r3]
  | (_,false),(_,false) -> assert false


(** [recon ctx expr] reconstruct a type of [expr], generating a set of
    constraints. *)
let constraints ctx ucs exp : (ty * constrs) =
  let is_scalar x =
    match Ctx.find ctx x with
    | Some (l,r) ->
      let constrs t = List.filter ucs ~f:(fun (ty,_) -> Dim.(t = ty)) in
      let has_one cs = List.exists cs ~f:(fun (_,c) -> Dim.(c = one)) in
      let l = constrs l in
      let r = constrs r in
      has_one l && has_one r
    | None -> false in
  let rec recon expr =
    let (lt,rt) as tt = Ctx.get_or_add_fresh ctx expr in
    match expr with
    | Num _ -> tt,[lt,one; rt,one]
    | Var _ -> tt,[]
    | Ind (s,i1,i2) ->
      let _,cs = recon s in
      tt, List.concat [
        (match i1,i2 with
         | None, Some _ -> [rt,one]
         | Some _, None -> [lt,one]
         | Some _, Some _ -> [rt,one; lt,one]
         | None, None -> []);
        cs
      ]
    | Bop (op,s1,s2) ->
      let t1,c1 = recon s1 in
      let t2,c2 = recon s2 in
      let cons =
        let s1_is_scalar = is_scalar s1 in
        let s2_is_scalar = is_scalar s2 in
        if op = Mul && (s1_is_scalar || s2_is_scalar)
        then scalar_constr (t1,s1_is_scalar) (t2,s2_is_scalar) tt
        else binary_constr t1 t2 tt op in
      tt, List.concat [cons; c1; c2]
    | Uop (op,s1) ->
      let t1,c1 = recon s1 in
      tt, List.concat [unary_constr t1 tt op; c1] in
  recon exp

module Subst = struct
  include Dim.Table

  let substitute (f,t) x = Dim.(if x = f then t else x)
  let replace t s = map t ~f:(substitute s)

  let extend_replace t (t1,t2) =
    add_exn t ~key:t1 ~data:t2;
    replace t (t1,t2)
end


exception Unification_failed of ty * dim Subst.t with sexp
let fail ty subst = raise (Unification_failed (ty,subst))

let swap (a,b) = (b,a)


let rec unify cs subst =
  match cs with
  | [] -> subst
  | (x,y)::cs when x = y -> unify cs subst
  | ((IConst _, IConst _)       as ty) :: _
  | ((IConst _, INum _)         as ty) :: _
  | ((INum _,   IConst _ )      as ty) :: _
  | ((INum _,   INum _)         as ty) :: _ -> fail ty subst
  | ((IConst _ |INum _),IVar _) as c::cs -> unify (swap c :: cs) subst
  | (IVar _ as lhs,rhs)::cs -> match Subst.find subst lhs with
    | None -> unify cs (Subst.extend_replace subst (lhs,rhs))
    | Some v -> unify ((rhs,v) :: cs) subst

let union_find subst =
  Subst.fold subst ~init:(UnionFind.create ())
    ~f:(fun ~key:lhs ~data:rhs set -> UnionFind.union set lhs rhs)

let infer (script : script) : subst =
  let env,ucs = init script in
  let cs = List.map script ~f:(function
      | Some exp,_ -> snd(constraints env ucs exp)
      | None,_ -> []) in
  let cs = List.concat (ucs :: cs) |>
           List.sort ~cmp:compare  |>
           List.remove_consecutive_duplicates ~equal:(=) in
  let subst = Subst.create () in
  try
    let subst = unify cs subst in
    let ds = union_find subst in
    Ctx.create_substitution env ds
  with Unification_failed ((d1,d2),subst) ->
    let ds = union_find subst in
    raise (Type_error (d1, d2, Ctx.create_substitution env ds))

let is_scalar (d1,d2) = Dim.(d1 = one && d2 = one)
let is_vector (d1,d2) = Dim.(d1 <> one && d2 = one)

let ppr_error d1 d2 subst : ppr =
  let open Printer in
  let title =
    string "Failing due to a type error: " ++
    Dim.ppr d1 ++ string " <> " ++
    Dim.ppr d2 ++ string ", " ++
    flush ++ string "where" in
  let subst = Exp.Map.to_alist subst |> List.map ~f:(fun (e,(d1,d2)) ->
      Exp.ppr e  ++ string " : " ++ string "{" ++
      Dim.ppr d1 ++ string "," ++ Dim.ppr d2 ++ string "}")  in
  let sep = flush ++ string "      " in
  title ++ sep ++ list 0.1 sep subst ++ flush
