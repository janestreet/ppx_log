open! Base
open! Import

type t =
  [ `Debug
  | `Info
  | `Warn
  | `Error
  ]
[@@deriving sexp_of]
