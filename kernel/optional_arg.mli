open! Base
open! Import
open Ppxlib

(** Represents the possible values for an optional argument. *)
type t = [ `Labelled of expression | `Optional of expression ] option

val of_optional_expr : expression option -> t
val to_expr : t -> loc:location -> expression
val to_arg : t -> name:label -> (arg_label * expression) option
