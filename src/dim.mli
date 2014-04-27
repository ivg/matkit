open Core.Std
open Ast

type t = dim with sexp,compare
include Comparable  with type t := t
include Hashable    with type t := t
include Stringable  with type t := t
