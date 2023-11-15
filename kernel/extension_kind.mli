open! Base
open! Import
open Ppxlib

module Level : sig
  type t =
    [ `Debug
    | `Info
    | `Error
    ]
  [@@deriving enumerate]

  val to_expression : t -> loc:location -> expression
end

module Format : sig
  type t =
    [ `String
    | `Message
    | `Sexp
    | `Printf
    ]
end

type t =
  { level : Level.t option
  ; log_kind : unit Log_kind.t
  ; format : Format.t
  }
[@@deriving enumerate]

val name : t -> label
