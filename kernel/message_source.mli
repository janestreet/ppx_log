open! Base
open! Import
open! Ppxlib

type t = { loc : location }

val render : t -> expression
val of_source_code_position_expr : expression -> expression
