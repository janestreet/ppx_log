open! Base
open! Import

type t =
  [ `Debug
  | `Info
  | `Error
  ]
[@@deriving sexp_of]
