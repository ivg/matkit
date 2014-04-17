(** A definition of abstract syntax tree. *)

open Core.Std




module Base = struct
  module Sym = String
  type sym = Sym.t with sexp,compare
  (** a type to represent symbol (still not sure what to use)  *)

  (** binary operations  *)
  type binary =
    | Mul        (** an overloaded multiplication  *)
    | Sub        (** subtraction  *)
    | Add        (** Addition  *)
    | Pow        (** Power *)
    | Had        (** Hadamard multiplication  *)
  with sexp, compare

  (** unary operations  *)
  type unary =
    | Tran      (** Transpose (and possibly conjugate)   *)
    | Conj      (** Conjugate (without a transposition)  *)
    | UNeg      (** Negation  *)
  with sexp, compare

  type nat1 = One
            | Succ of nat1
  with sexp, compare

  (** a type to denote matrix indices  *)
  type index = INum of nat1      (** constant index    *)
             | IVar of sym       (** variable index    *)
  with sexp, compare

  (** AST type *)
  type t =
    | Num of int                              (** Numeric constant  *)
    | Var of sym                              (** A variable        *)
    | Uop of unary  * t                       (** Unary operation   *)
    | Bop of binary * t * t                   (** Binary operation  *)
    | Sub of t * index option * index option  (** Indexing  *)
  with sexp, compare, variants
  let hash = Hashtbl.hash
end

include Base
module Ast = struct
  include Comparable.Make(Base)
  include Hashable.Make(Base)
end
