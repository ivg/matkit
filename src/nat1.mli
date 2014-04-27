open Core.Std

type t with sexp,compare
val one: t
include Intable    with type t := t
include Hashable   with type t := t
include Comparable with type t := t
include Stringable with type t := t
