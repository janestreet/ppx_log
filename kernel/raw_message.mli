open! Base
open! Import
open! Ppxlib

type t

val pattern : unit -> (payload, t -> 'a, 'a) Ast_pattern.t
val render : t -> expression
