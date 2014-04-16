open Core.Std
open Ast
open Type


let fresh_index = IVar "random symbol!11"

let new_mat () = tmat fresh_index fresh_index

let rec recons ctx expr : (ty * constrs) = match expr with
  | Num _ -> TNum,[]
  | Var id -> Env.get_or_add_fresh ctx id,[]
  | Bop (Mul,s1,s2) ->
    let t1,c1 = recons ctx s1 in
    let t2,c2 = recons ctx s2 in
    let ty = new_mat () in
    ty, List.concat Constr.([[lcomm t1 ty]; [rcomm t2 ty] ;c1; c2])
  | _ -> assert false
