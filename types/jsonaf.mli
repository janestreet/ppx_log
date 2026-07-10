@@ portable

open! Base
open! Import

type t : immutable_data =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]
[@@deriving sexp_of ~stackify]
