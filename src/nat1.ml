open Core.Std

module T = struct
  type t = One
         | Succ of t
  with sexp, compare
  let hash = Hashtbl.hash
end

include T

let one = One

let rec of_int_exn = function
  | n when n <= 0 -> invalid_arg "nat1 should be positive"
  | 1 -> one
  | n -> Succ (of_int_exn (n-1))

let to_int_exn init =
  let rec loop n nat =
    if n = Int.max_value
    then invalid_arg "Number is too big"
    else match nat with
      | One -> 1 + n
      | Succ nat -> loop (n+1) nat in
  loop 0 init

let to_string n = Int.to_string (to_int_exn n)
let of_string s = Int.of_string s |> of_int_exn

include Comparable.Make(T)
include Hashable.Make(T)
