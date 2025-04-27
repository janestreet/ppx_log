open! Base
open! Import
open! Ppxlib

type t = { loc : location }

let render { loc } =
  let open (val Ast_builder.make loc) in
  [%expr
    Ppx_log_types.Message_source.Private.code
      ~pos_fname:[%e estring (Ppx_here_expander.expand_filename loc.loc_start.pos_fname)]
      ~pos_lnum:[%e eint loc.loc_start.pos_lnum]
      ~module_name:Stdlib.__MODULE__ [@merlin.hide]]
;;

let of_source_code_position_expr (expr : expression) =
  let open (val Ast_builder.make expr.pexp_loc) in
  [%expr
    Ppx_log_types.Message_source.Private.of_source_code_position [%e expr] [@merlin.hide]]
;;
