open! Base
open! Import

type t =
  [ `Sexp of Sexp.t
  | `String of string
  | `Structured of Message_sexp.t
  ]

val sexp_of_t_hum : t -> Sexp.t

module Unstable : sig
  type nonrec t = t [@@deriving sexp_of]
end
