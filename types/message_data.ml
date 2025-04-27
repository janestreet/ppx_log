open! Base
open! Import

type t =
  [ `Sexp of Sexp.t
  | `String of string
  | `Structured of Message_sexp.t
  ]

let sexp_of_t_hum = function
  | `String s -> [%sexp (s : string)]
  | `Sexp s -> s
  | `Structured s -> Message_sexp.render s
;;

module Unstable = struct
  type nonrec t =
    [ `Sexp of Sexp.t
    | `String of string
    | `Structured of Message_sexp.Unstable.t
    ]
  [@@deriving sexp_of]
end
