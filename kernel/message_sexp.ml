open! Base
open! Import
open Ppxlib

type t =
  { message_label : [ `Literal of constant | `String_expr of expression ] option
  ; tags : (arg_label * expression) list
  ; loc : location
  }

let of_extension_payload extension_payload ~loc =
  let message_label, tags =
    match Extension_payload.to_args extension_payload with
    | hd :: tl ->
      (* The first argument may be a tag, nothing, or a label. *)
      (match hd with
       (* [%log log "" ...] has "" ignored *)
       | Nolabel, [%expr ""] -> None, tl
       (* [%log log (a : t1) (b : t2)] treats a and b both as tags *)
       | _, [%expr ([%e? _] : [%t? _])]
       (* [%log log ~label:...] treats the the first arg as a tag as well *)
       | (Labelled (_ : string) | Optional (_ : string)), (_ : expression)
       (* [%message] has a special case for [%here]. We can interpret it as a tag. *)
       | Nolabel, [%expr [%here]] -> None, hd :: tl
       (* Unlabelled literals can be interpreted as a label. These should only be
          strings in practice. *)
       | Nolabel, { pexp_desc = Pexp_constant c; _ } -> Some (`Literal c), tl
       (* Otherwise, [%log [%e e]] ought to be an unlabelled expression, in which case it
          can be interpreted as a string label payload. *)
       | Nolabel, hd -> Some (`String_expr hd), tl)
    | [] -> None, []
  in
  { message_label; tags; loc }
;;

let constant_to_string_expr constant ~loc =
  let open (val Ast_builder.make loc) in
  match constant with
  | Pconst_string _ as string_constant -> pexp_constant string_constant
  | _ ->
    [%expr
      match
        [%e
          Ppx_sexp_message_expander.sexp_of_labelled_exprs
            [ Nolabel, pexp_constant constant ]
            ~omit_nil:false
            ~loc]
      with
      | Atom x -> x
      | List _ -> assert false]
;;

let render_message_label ~loc = function
  | None -> [%expr None]
  | Some (`Literal const) ->
    [%expr Some (String_literal [%e constant_to_string_expr const ~loc])]
  | Some (`String_expr expr) -> [%expr Some (String [%e expr])]
;;

let render { message_label; tags; loc } ~render_with_additional_parentheses =
  let message_label = render_message_label message_label ~loc in
  let tags = List.map tags ~f:Log_tag.parse_arg |> Log_tag.render_list ~loc in
  if render_with_additional_parentheses
  then
    [%expr
      Ppx_log_types.Message_sexp.create
        [%e message_label]
        ~tags:[%e tags]
        ~legacy_render_with_additional_parentheses:true]
  else [%expr Ppx_log_types.Message_sexp.create [%e message_label] ~tags:[%e tags]]
;;
