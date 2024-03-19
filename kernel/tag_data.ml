open! Base
open! Import
open! Ppxlib_with_sexp
open! Ast_builder.Default

type t =
  | Constant of constant
  | Type_constrained of expression * core_type
  | String_expression of expression
  | Here_extension
[@@deriving sexp_of]

let empty_attr name =
  Attribute.declare ("log." ^ name) Attribute.Context.core_type Ast_pattern.(pstr nil) ()
;;

let omit_nil_attr = empty_attr "sexp.omit_nil"
let option_attr = empty_attr "sexp.option"

let parse e =
  let t =
    match e with
    | { pexp_desc = Pexp_constant constant; _ } -> Constant constant
    | { pexp_desc = Pexp_constraint (expr, ctyp); _ } -> Type_constrained (expr, ctyp)
    | [%expr [%here]] -> Here_extension
    | e -> String_expression e
  in
  { txt = t; loc = e.pexp_loc }
;;

let type_labelled_constant ~loc const =
  let e = pexp_constant ~loc const in
  match const with
  | Pconst_integer (_ : string * char option) -> [%expr Int [%e e]]
  | Pconst_char (_ : char) -> [%expr Char [%e e]]
  | Pconst_string (_ : string * location * string option) -> [%expr String [%e e]]
  | Pconst_float (_ : string * char option) -> [%expr Float [%e e]]
;;

let sexp_of_constraint ~loc expr ctyp =
  let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp in
  eapply ~loc sexp_of [ expr ]
;;

let log_json_attribute =
  Attribute.declare "@j" Attribute.Context.core_type Ast_pattern.(pstr nil) ()
;;

(* [omit_nil_expr] and [sexp_option_expr] are roughly copied from
   [ppx_sexp_message_expander]. *)
let omit_nil_expr expr ctyp ~loc =
  ( `Tag_option
  , [%expr
      match [%e sexp_of_constraint ~loc expr ctyp] with
      | Sexp.List [] -> None
      | sexp -> Some (Ppx_log_types.Tag_data.Sexp sexp)] )
;;

let sexp_option_expr expr ~type_without_option:typ ~loc =
  ( `Tag_option
  , [%expr
      match [%e expr] with
      | None -> None
      | Some value ->
        Some
          (Ppx_log_types.Tag_data.Sexp
             ([%e Ppx_sexp_conv_expander.Sexp_of.core_type typ] value))] )
;;

let type_labelled_constraint ~loc expr ctyp =
  let default () =
    match Attribute.consume log_json_attribute ctyp with
    | Some (ctyp, ()) -> `Tag, [%expr Json ([%jsonaf_of: [%t ctyp]] [%e expr])]
    | None ->
      (match Attribute.get omit_nil_attr ctyp with
       | Some () -> omit_nil_expr expr ctyp ~loc
       | None -> `Tag, [%expr Sexp [%e sexp_of_constraint ~loc expr ctyp]])
  in
  match ctyp with
  | [%type: int] -> `Tag, [%expr Int [%e expr]]
  | [%type: string] -> `Tag, [%expr String [%e expr]]
  | [%type: float] -> `Tag, [%expr Float [%e expr]]
  | [%type: char] -> `Tag, [%expr Char [%e expr]]
  | [%type: bool] -> `Tag, [%expr Bool [%e expr]]
  | [%type: [%t? type_without_option] option] ->
    (match Attribute.get option_attr ctyp with
     | Some () -> sexp_option_expr expr ~type_without_option ~loc
     | None -> default ())
  | _ -> default ()
;;

let render { txt = t; loc } =
  match t with
  | Constant const -> `Tag, type_labelled_constant ~loc const
  | Type_constrained (expr, ctyp) -> type_labelled_constraint ~loc expr ctyp
  | String_expression e -> `Tag, [%expr String [%e e]]
  | Here_extension ->
    `Tag, [%expr String [%e Ppx_here_expander.lift_position_as_string ~loc]]
;;
