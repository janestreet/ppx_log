open! Base
open! Import
open Ppxlib

type 'data t =
  [ `Global
  | `Instance of 'data
  ]
[@@deriving enumerate]

val extension_prefix : _ t -> string
val would_log : expression t -> level:Optional_arg.t -> loc:location -> expression
val log_function : _ t -> loc:location -> expression
val log_default : _ t -> loc:location -> Parsetree.expression
val log_arg : expression t -> (arg_label * expression) option
