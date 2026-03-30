open! Base
open! Import

type t =
  [ `Debug
  | `Info (** default level *)
  | `Warn
  | `Error
  ]
[@@deriving sexp_of]
