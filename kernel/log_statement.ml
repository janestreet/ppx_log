open! Base
open! Import
open Ppxlib

type t =
  { format : [ `Message_with_extra_tag_parentheses | Extension_kind.Format.t ]
  ; log : expression Log_kind.t
  ; args : Extension_payload.t
  ; level : Optional_arg.t
  ; time : Optional_arg.t
  ; legacy_tags : Optional_arg.t
  ; loc_override : expression option
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
  ; loc_attr
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
    | (`Printf | `Sexp | `String | `Raw) as format -> format
  in
  let log, args =
    match log_kind with
    | `Global | `Explicit_global -> `Global, Extension_payload.Expression args
    | `Instance () ->
      Ast_pattern.(parse (pexp_apply __ __)) loc args (fun log_expr args ->
        `Instance log_expr, Extension_payload.Args args)
  in
  { format; log; args; level; time; legacy_tags; loc_override = loc_attr; loc }
;;

let message_data
  (format : [< `Message | `Message_with_extra_tag_parentheses | `Sexp | `String ])
  extension_payload
  ~loc
  =
  let open (val Ast_builder.make loc) in
  match format with
  | (`Message | `Message_with_extra_tag_parentheses) as format ->
    let render_with_additional_parentheses =
      match format with
      | `Message -> false
      | `Message_with_extra_tag_parentheses -> true
    in
    let payload =
      Message_sexp.of_extension_payload extension_payload ~loc
      |> Message_sexp.render ~render_with_additional_parentheses
    in
    [%expr `Structured [%e payload]]
  | `Sexp ->
    let payload =
      match Extension_payload.single_expression_or_error extension_payload ~loc with
      (* [%log.global.sexp (... : T.t)] uses [T.sexp_of_t]. *)
      | [%expr ([%e? expr] : [%t? typ])] ->
        let sexp_of_fn = Ppx_sexp_conv_expander.Sexp_of.core_type typ ~stackify:false in
        Ast_builder.Default.eapply sexp_of_fn [ expr ] ~loc
      (* [%log.global.sexp my_expr] assumes [my_expr] is a [Sexp.t]. *)
      | expr -> expr
    in
    [%expr `Sexp [%e payload]]
  | `String ->
    let payload =
      match Extension_payload.single_expression_or_error extension_payload ~loc with
      | { pexp_desc = Pexp_constant (Pconst_string (s, loc, delimiter)); pexp_loc; _ } ->
        Ppx_string.expand
          ~config:(Ppx_string.config_for_string Global_input_heap_output)
          ~expr_loc:pexp_loc
          ~string_loc:loc
          ~string:s
          ~delimiter
        |> Merlin_helpers.hide_expression
      | expr -> expr
    in
    [%expr `String [%e payload]]
;;

(* The below is copied from [ppx_sexp_message_expander] - see there for reasoning. *)
let wrap_in_cold_function ~loc expr =
  [%expr
    (let[@cold] ppx_log_statement () = [%e expr] in
     ppx_log_statement () [@nontail])
    [@merlin.hide]]
;;

let get_tuple_from_expression extension_payload ~loc =
  let open (val Ast_builder.make loc) in
  let payload = Extension_payload.single_expression_or_error extension_payload ~loc in
  let source_label = "message_source" in
  let data_label = "message_data" in
  let tuple_element_patterns =
    [ Ast_builder.Default.ppat_var ~loc (Located.mk source_label)
    ; Ast_builder.Default.ppat_var ~loc (Located.mk data_label)
    ]
  in
  let tuple_pattern = Ast_builder.Default.ppat_tuple ~loc tuple_element_patterns in
  let build_tuple_element_node ~var_to_extract =
    Ast_builder.Default.pexp_match
      ~loc
      payload
      (* match payload against a pattern (lhs) defining a tuple. If it matches, return an
         expression (rhs) that is the variable corresponding to the fst or snd element of
         the tuple.
      *)
      [ case ~lhs:tuple_pattern ~rhs:(evar var_to_extract) ~guard:None ]
  in
  ( build_tuple_element_node ~var_to_extract:source_label
  , build_tuple_element_node ~var_to_extract:data_label )
;;

let render { format; log; args; level; time; legacy_tags; loc_override; loc } =
  let open (val Ast_builder.make loc) in
  let function_name = Log_kind.log_function log ~loc in
  let make_log_statement ?source data () =
    let source =
      match source with
      | Some x -> x
      | None ->
        (match loc_override with
         | None -> Message_source.render { loc }
         | Some expr -> Message_source.of_source_code_position_expr expr)
    in
    List.filter_opt
      [ Optional_arg.to_arg level ~name:"level"
      ; Optional_arg.to_arg time ~name:"time"
      ; Optional_arg.to_arg legacy_tags ~name:"tags"
      ; Log_kind.log_arg log
      ; Some (Nolabel, data)
      ; Some (Nolabel, source)
      ]
    |> Ast_builder.Default.pexp_apply function_name ~loc
  in
  let log_statement =
    match format with
    | `Printf ->
      (* Printf is handled with a ksprintf, so it's separated out from [message_data]. We
         could maybe use a [sprintf] instead, but it causes format strings to have a
         slightly different type (the last type parameter which represents the return
         value of the format string is a string, not unit).

         The log statement itself returns unit, so I think unit makes sense here. *)
      pexp_apply
        [%expr
          Printf.ksprintf (fun str -> [%e make_log_statement [%expr `String str] ()])]
        (Extension_payload.to_args args)
    | `Raw ->
      (* [%log.global.raw* my_expr] assumes [my_expr] is a
         [Message_source.t * Message_data.t] *)
      let source, data = get_tuple_from_expression args ~loc in
      make_log_statement ~source data ()
    | (`String | `Sexp | `Message | `Message_with_extra_tag_parentheses) as format ->
      make_log_statement (message_data format args ~loc) ()
  in
  [%expr
    if [%e Log_kind.would_log log ~level ~loc] [@merlin.hide]
    then [%e wrap_in_cold_function log_statement ~loc]
    else [%e Log_kind.log_default log ~loc]]
;;
