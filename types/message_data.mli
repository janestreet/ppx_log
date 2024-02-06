open! Base
open! Import

type t =
  [ `Sexp of Sexp.t
  | `String of string
  | `Structured of Message_sexp.t
  ]

module Unstable : sig
  type nonrec t = t [@@deriving sexp_of]
end
