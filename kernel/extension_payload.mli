open! Base
open! Import
open Ppxlib

(* This module represents the fact that the expression passed to the ppx may be either
   interpreted as an arg list (e.g., [%log.info log "msg" (a1 : t1) ...] has the args as a
   list ["msg"; (a1 : t1)]) vs. a pexp_apply [%log.global.info "msg" (a1 : t1)] ((a1 : t1)
   is applied to "msg").

   We can't flatten it to an arg list immediately because in the [Sexp] format, a
   pexp_apply we want to keep as a pexp_apply. *)

type t =
  | Args of (arg_label * expression) list
  | Expression of expression

val to_args : t -> (arg_label * expression) list
val single_expression_or_error : t -> loc:location -> expression
