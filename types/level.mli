open! Base
open! Import

type t =
  [ `Debug
  | `Info (** default level *)
  | `Error
  ]
[@@deriving sexp_of]
