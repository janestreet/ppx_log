open! Base
open! Import
open Ppxlib

type t =
  { format : [ `Message_with_extra_tag_parentheses | Extension_kind.Format.t ]
  ; log : [ `Global | `Instance of expression ]
  ; args : Extension_payload.t
  ; level : Optional_arg.t
  ; time : Optional_arg.t
  ; legacy_tags : Optional_arg.t
  ; loc : location
  }

let level_arg ~level_attr ~extension_name_level ~loc =
  match level_attr, extension_name_level with
  | None, None -> None
  | None, Some level -> Some (`Labelled (Extension_kind.Level.to_expression level ~loc))
  | Some expr, None -> Some (`Optional expr)
  | Some _, Some _ ->
    Some
      (`Labelled
        (Ast_builder.Default.pexp_extension
           ~loc
           (Location.error_extensionf
              ~loc
              "Cannot provide a [@@level] attribute if the extension name already has a \
               level")))
;;

let tags ~tags_attr ~loc ~log_source_position =
  let pos = Ppx_here_expander.lift_position ~loc in
  let pos_tag = [%expr [ "pos", Source_code_position.to_string [%e pos] ]] in
  let src_tags = if log_source_position then [ pos_tag ] else [] in
  let attr_tags = tags_attr |> Option.to_list in
  match src_tags @ attr_tags with
  | [] -> None
  | [ e ] -> Some (`Labelled e)
  | _ :: _ as es ->
    Some (`Labelled [%expr List.concat [%e Ast_builder.Default.elist ~loc es]])
;;

let create
  { Extension_kind.format; log_kind; level = extension_name_level }
  { Parsed_extension.args
  ; tags_attr
  ; level_attr
  ; time_attr
  ; legacy_add_extra_tag_parentheses
  }
  ~loc
  ~log_source_position
  =
  let loc = { loc with loc_ghost = true } in
  let legacy_tags = tags ~tags_attr ~loc ~log_source_position in
  let level = level_arg ~level_attr ~extension_name_level ~loc in
  let time = Optional_arg.of_optional_expr time_attr in
  let format =
    match format with
    | `Message ->
      if legacy_add_extra_tag_parentheses
      then `Message_with_extra_tag_parentheses
      else `Message
    | (`Printf | `Sexp | `String) as format -> format
  in
  let log, args =
    match log_kind with
    | `Global -> `Global, Extension_payload.Expression args
    | `Instance () ->
      Ast_pattern.(parse (pexp_apply __ __)) loc args (fun log_expr args ->
        `Instance log_expr, Extension_payload.Args args)
  in
  { format; log; args; level; time; legacy_tags; loc }
;;

let function_name_and_payload format extension_payload ~loc =
  match format with
  | (`Message | `Message_with_extra_tag_parentheses) as format ->
    let render_with_additional_parentheses =
      match format with
      | `Message -> false
      | `Message_with_extra_tag_parentheses -> true
    in
    let payload_args =
      Message_sexp.of_extension_payload extension_payload ~loc
      |> Message_sexp.payload_args ~render_with_additional_parentheses
    in
    `Message, payload_args
  | `Sexp ->
    let payload =
      match Extension_payload.single_expression_or_error extension_payload ~loc with
      (* [%log.global.sexp (... : T.t)] uses [T.sexp_of_t]. *)
      | [%expr ([%e? expr] : [%t? typ])] ->
        let sexp_of_fn = Ppx_sexp_conv_expander.Sexp_of.core_type typ in
        Ast_builder.Default.eapply sexp_of_fn [ expr ] ~loc
      (* [%log.global.sexp my_expr] assumes [my_expr] is a [Sexp.t].  *)
      | expr -> expr
    in
    `Sexp, [ Nolabel, payload ]
  | `Printf -> `Printf, Extension_payload.to_args extension_payload
  | `String ->
    let payload =
      match Extension_payload.single_expression_or_error extension_payload ~loc with
      | { pexp_desc = Pexp_constant (Pconst_string (s, loc, delimiter)); pexp_loc; _ } ->
        Ppx_string.expand ~expr_loc:pexp_loc ~string_loc:loc ~string:s ~delimiter
        |> Merlin_helpers.hide_expression
      | expr -> expr
    in
    `Printf, [ Nolabel, [%expr "%s"]; Nolabel, payload ]
;;

(* The below is copied from [ppx_sexp_message_expander] - see there for reasoning. *)
let wrap_in_cold_function ~loc expr =
  [%expr
    (let[@cold] ppx_log_statement () = [%e expr] in
     ppx_log_statement () [@nontail]) [@merlin.hide]]
;;

let render { format; log; args; level; time; legacy_tags; loc } =
  let name, payload_args = function_name_and_payload format args ~loc in
  let function_name = Log_kind.log_function log name ~loc in
  let log_statement =
    List.filter_opt
      [ Optional_arg.to_arg level ~name:"level"
      ; Optional_arg.to_arg time ~name:"time"
      ; Optional_arg.to_arg legacy_tags ~name:"tags"
      ; Log_kind.log_arg log
      ]
    @ payload_args
    |> Ast_builder.Default.pexp_apply function_name ~loc
  in
  [%expr
    if [%e Log_kind.would_log log ~level ~loc] [@merlin.hide]
    then [%e wrap_in_cold_function log_statement ~loc]
    else [%e Log_kind.log_default log ~loc]]
;;
