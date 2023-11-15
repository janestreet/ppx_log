open! Base
open! Import
open Ppxlib

let single_expr_attr name =
  Attribute.declare name Pstr_eval Ast_pattern.(single_expr_payload __) (fun x -> x)
;;

let tags_attr = single_expr_attr "tags"
let time_attr = single_expr_attr "time"
let level_attr = single_expr_attr "level"

let legacy_add_extra_tag_parentheses_attr =
  Attribute.declare "legacy_tag_parentheses" Pstr_eval Ast_pattern.(pstr nil) ()
;;

type t =
  { args : expression
  ; tags_attr : expression option
  ; level_attr : expression option
  ; time_attr : expression option
  ; legacy_add_extra_tag_parentheses : bool
  }

let pattern () =
  let open Ast_pattern in
  pstr (as__ (pstr_eval __ drop) ^:: nil)
  |> map ~f:(fun f decl args ->
       { args
       ; tags_attr = Attribute.get tags_attr decl
       ; level_attr = Attribute.get level_attr decl
       ; time_attr = Attribute.get time_attr decl
       ; legacy_add_extra_tag_parentheses =
           Attribute.get legacy_add_extra_tag_parentheses_attr decl |> Option.is_some
       }
       |> f)
;;
