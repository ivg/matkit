open Core.Std
open Ast
open Type




let (=~) (t1,s1) (t2,s2) = Commute (t1,s1,t2,s2)

let one = INum One
let scalar = (one, one)
let vector d1 = (d1, one)
let matrix d1 d2 = (d1,d2)

let equal_constr t1 t2 = [
  (t1,Lhs) =~ (t2,Lhs);
  (t1,Rhs) =~ (t2,Rhs);
]

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
      let ty = Env.get_or_add_fresh env exp in
      [Eq (ty,Rhs,one)]
    | Var _ -> [] in
  loop



let init exp cs =
  let env = Env.empty () in
  let cs = List.fold cs ~init:[] ~f:(fun constrs (sym,constr) ->
      let ty = Env.get_or_add_fresh env (Var (String.of_char sym)) in
      match type_of_string constr with
      | `Matrix -> constrs
      | `Vector -> Eq (ty,Rhs,one) :: constrs
      | `Scalar -> Eq (ty,Rhs,one) :: Eq (ty,Lhs,one) :: constrs
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
let binary_constr t1 t2 ty = function
  | Mul -> [
      (t1,Lhs) =~ (ty,Lhs);
      (t2,Rhs) =~ (ty,Rhs);
      (t1,Rhs) =~ (t2,Lhs)
    ]
  | (Sub|Add|Had) -> List.concat [
      equal_constr t1 t2;
      equal_constr t1 ty;
      equal_constr t2 ty;
    ]
  | Pow -> List.concat [
      equal_constr t1 scalar;
      equal_constr t2 ty;
    ]

let unary_constr t1 ty = function
  | Tran -> [
      (t1,Lhs) =~ (ty,Rhs);
      (t1,Rhs) =~ (ty,Lhs);
    ]
  | (UNeg|Conj) -> equal_constr t1 ty

let rec recon ctx expr : (ty * constrs) =
  let ty = Env.get_or_add_fresh ctx expr in
  match expr with
  | Num _ ->  scalar ,[]
  | Var id -> ty,[]
  | Sub (s,i1,i2) ->
    let ts,cs = recon ctx s in
    ty, List.concat [
      (match i1,i2 with
      | None, Some _ -> [Eq (ty,Rhs,one)]
      | Some _, None -> [Eq (ty,Lhs,one)]
      | Some _, Some _ -> [
          Eq (ty,Rhs,one);
          Eq (ty,Lhs,one)
        ]
      | None, None -> []);
      cs
    ]
  | Bop (op,s1,s2) ->
    let t1,c1 = recon ctx s1 in
    let t2,c2 = recon ctx s2 in
    ty, List.concat [binary_constr t1 t2 ty op; c1; c2]
  | Uop (op,s1) ->
    let t1,c1 = recon ctx s1 in
    ty,List.concat [unary_constr t1 ty op; c1]


let infer env expr =
  let (ty,_constrs) = recon env expr in
  ty
