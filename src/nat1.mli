open Core.Std
open Ast

type t = nat1 with sexp,compare
include Intable    with type t := t
include Hashable   with type t := t
include Comparable with type t := t
include Stringable with type t := t
