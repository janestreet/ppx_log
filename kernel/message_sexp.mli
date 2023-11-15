open! Base
open! Import
open Ppxlib

(** Represents the data relevant for constructing a message sexp for a log statement. *)
type t

val of_extension_payload
  :  Extension_payload.t
  -> render_with_additional_parentheses:bool
  -> t

val payload_args : t -> loc:location -> (arg_label * expression) list
