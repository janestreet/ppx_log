open! Base
open! Import
open Ppxlib

(** Represents the data relevant for constructing a [Ppx_log_types.Message_sexp.t]. *)
type t

val of_extension_payload : Extension_payload.t -> loc:location -> t

val payload_args
  :  t
  -> render_with_additional_parentheses:bool
  -> (arg_label * expression) list
