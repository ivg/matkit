(** A definition of abstract syntax tree. *)

open Core.Std

(** binary operations  *)
type binary =
  | Mul        (** an overloaded multiplication  *)
  | Sub        (** subtraction  *)
  | Add        (** Addition  *)
  | Pow        (** Power *)
  | Had        (** Hadamard multiplication  *)
with sexp

(** unary operations  *)
type unary =
  | Tran      (** Transpose (and possibly conjugate)   *)
  | Conj      (** Conjugate (without a transposition)  *)
  | UNeg      (** Negation  *)
with sexp


(** a type to denote matrix indices  *)
type index = Num of int | Sym of string with sexp

type t =
  | Var of char                (** A variable        *)
  | Uop of unary  * t          (** Unary operation   *)
  | Bop of binary * t * t      (** Binary operation  *)
  | Sub of t * index list      (** Indexing  *)
with sexp

