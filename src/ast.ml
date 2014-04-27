(** A definition of abstract syntax tree. *)

open Core.Std

module Sym = String
type sym = Sym.t with sexp,compare

(** a type to represent symbol (still not sure what to use)  *)

(** binary operations  *)
type binary =
  | Eql        (** Equality  *)
  | Mul        (** Overloaded multiplication  *)
  | Div        (** Division  *)
  | Sub        (** Subtraction  *)
  | Add        (** Addition  *)
  | Pow        (** Power  *)
  | Had        (** Hadamard multiplication  *)
  | HDiv       (** Hadamard division  *)
  | HPow       (** Hadamard power  *)
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
type dim = INum of nat1      (** constant dim    *)
         | IVar of sym       (** variable dim    *)
         | IConst of sym     (** rigid dim       *)
with sexp, compare




(** AST type *)
type exp =
  | Num of int                                (** Numeric constant  *)
  | Var of sym                                (** A variable        *)
  | Uop of unary  * exp                       (** Unary operation   *)
  | Bop of binary * exp * exp                 (** Binary operation  *)
  | Ind of exp * dim option * dim option      (** Indexing  *)
with sexp, compare, variants


module Ring = struct
  type t = Z | R | C with sexp
  let ring_of_char c =
    match c with
    | 'Z' | 'z' -> Z
    | 'C' | 'c' -> C
    | 'R' | 'r' -> R
    | _ -> R (* should generate a warning at least *)
end

type kind = (char * string) with sexp

type ring = Ring.t * (dim * dim) option with sexp



