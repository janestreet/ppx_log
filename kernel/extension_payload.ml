open! Base
open! Import
open Ppxlib

type t =
  | Args of (arg_label * expression) list
  | Expression of expression

let to_args = function
  | Args args -> args
  | Expression expr ->
    (match expr.pexp_desc with
     | Pexp_apply (expr, args) -> (Nolabel, expr) :: args
     | (_ : expression_desc) -> [ Nolabel, expr ])
;;

let single_expression_or_error t ~loc =
  match t with
  | Args [ (Nolabel, expr) ] | Expression expr -> expr
  | (_ : t) ->
    Ast_builder.Default.pexp_extension
      ~loc
      (Location.error_extensionf
         ~loc
         "Expected exactly one unlabelled argument as payload")
;;
