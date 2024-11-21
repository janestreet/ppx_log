open! Base
open! Import
open! Ppxlib

type t =
  { args : Extension_payload.t
  ; loc : location
  }

let create args ~loc =
  let loc = { loc with loc_ghost = true } in
  let args = Extension_payload.Expression args in
  { loc; args }
;;

let pattern () =
  let open Ast_pattern in
  pstr (pstr_eval __ drop ^:: nil) |> map' ~f:(fun loc f args -> f (create args ~loc))
;;

let render { args; loc } =
  let open (val Ast_builder.make loc) in
  let message_sexp_expr =
    Message_sexp.of_extension_payload args ~loc
    |> Message_sexp.render ~render_with_additional_parentheses:false
  in
  [%expr [%e Message_source.render { loc }], `Structured [%e message_sexp_expr]]
;;
