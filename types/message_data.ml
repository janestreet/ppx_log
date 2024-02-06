open! Base
open! Import

type t =
  [ `Sexp of Sexp.t
  | `String of string
  | `Structured of Message_sexp.t
  ]

module Unstable = struct
  type nonrec t =
    [ `Sexp of Sexp.t
    | `String of string
    | `Structured of Message_sexp.Unstable.t
    ]
  [@@deriving sexp_of]
end
