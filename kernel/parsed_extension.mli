open! Base
open! Import
open Ppxlib

type t =
  { args : expression
  ; tags_attr : expression option
  ; level_attr : expression option
  ; time_attr : expression option
  ; legacy_add_extra_tag_parentheses : bool
  }

val pattern : unit -> (payload, t -> 'a, 'a) Ast_pattern.t
