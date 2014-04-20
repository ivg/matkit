(** A definition of abstract syntax tree. *)

open Core.Std

let string_of_char = String.of_char 

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

(** positive natural numbers  *)
type nat1 = One
          | Succ of nat1
with sexp, compare


(** a type to denote matrix indices  *)
type index = INum of nat1      (** constant index    *)
           | IVar of sym       (** variable index    *)
with sexp, compare

module Index = struct
  module T = struct
    type t = index with sexp,compare
    let hash = Hashtbl.hash
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make(T)
end


(** AST type *)
type exp =
  | Num of int                                (** Numeric constant  *)
  | Var of sym                                (** A variable        *)
  | Uop of unary  * exp                       (** Unary operation   *)
  | Bop of binary * exp * exp                 (** Binary operation  *)
  | Ind of exp * index option * index option  (** Indexing  *)
with sexp, compare, variants
