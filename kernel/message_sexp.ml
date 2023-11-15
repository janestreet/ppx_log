open! Base
open! Import
open Ppxlib

(** Represents the data relevant for constructing a message sexp for a log statement. *)
type t =
  | Regular_message of Extension_payload.t
  | Message_with_extra_tag_parentheses of
      { message : expression option
      ; tags : (arg_label * expression) list
      }
[@@deriving variants]

let parse_message_and_tags extension_payload =
  (* For compatibility with [%sexp "message", { a1 : t1; a2 : t2 }] producing sexps
     where the tags are in an extra layer of parentheses (message ((a1 .) (a2 .))), we
     have this case.

     [%message] has some magic in 2 cases:
     - "" = Nothing at all. So:
       [%message "hello" ~i:(3 : int) ~j:(3 : int)] -> (hello (i 3) (j 3))
       [%message "" ~i:(3 : int) ~j:(3 : int)] -> ((i 3) (j 3))
     - If the final output has format (<one-elt>), the outer parentheses are removed.
       [%message "hello"] -> hello, NOT (hello)
       [%message "" ~i:(3 : int)] -> (i 3), NOT ((i 3))
     - However, [%message ""] = () = Sexp.List [].

     This makes it hard to directly use [Ppx_sexp_message_expander]. Instead I think
     it's most straightforward to feed the arguments in one by one, handle the first
     argument explicitly, and add parens afterwards. *)
  match Extension_payload.to_args extension_payload with
  | hd :: tl ->
    (* The first argument may be a tag, nothing, or a message. *)
    (match hd with
     (* [%log log "" ...] has "" ignored *)
     | Nolabel, [%expr ""] -> None, tl
     (* [%log log (a : t1) (b : t2)] treats a and b both as tags *)
     | _, [%expr ([%e? _] : [%t? _])]
     (* [%log log ~label:...] treats the the first arg as a tag as well *)
     | (Labelled (_ : string) | Optional (_ : string)), (_ : expression) -> None, hd :: tl
     (* Otherwise, [%log [%e e]] ought to be either a constant or an unlabelled
        expression, in which case it can be interpreted as a message payload. *)
     | Nolabel, hd -> Some hd, tl)
  | [] -> None, []
;;

let of_extension_payload extension_payload ~render_with_additional_parentheses =
  if render_with_additional_parentheses
  then (
    let message, tags = parse_message_and_tags extension_payload in
    Message_with_extra_tag_parentheses { message; tags })
  else Regular_message extension_payload
;;

let expand e ~loc =
  Ppx_sexp_message_expander.sexp_of_labelled_exprs [ e ] ~omit_nil:false ~loc
;;

let payload_args t ~loc =
  match t with
  | Regular_message extension_payload ->
    let sexp =
      Extension_payload.to_args extension_payload
      |> Ppx_sexp_message_expander.sexp_of_labelled_exprs ~omit_nil:false ~loc
    in
    [ Nolabel, sexp ]
  | Message_with_extra_tag_parentheses { message; tags } ->
    let tags =
      match tags with
      | [] -> None
      | _ :: _ as tags ->
        let tags = List.map tags ~f:(expand ~loc) |> Ast_builder.Default.elist ~loc in
        Some [%expr Core.Sexp.List [%e tags]]
    in
    let sexp =
      match message, tags with
      | None, None -> [%expr Core.Sexp.List []]
      | None, Some tags -> tags
      | Some msg, None -> expand (Nolabel, msg) ~loc
      | Some msg, Some tags ->
        [%expr Core.Sexp.List [ [%e expand (Nolabel, msg) ~loc]; [%e tags] ]]
    in
    [ Nolabel, sexp ]
;;
