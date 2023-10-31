open! Base
open Ppxlib

let log_source_position = ref false

let () =
  Driver.add_arg
    "-log-source-position"
    (Set log_source_position)
    ~doc:
      " If set, adds a \"pos\" tag with a source code position to every logged message."
;;

let single_expr_attr name =
  Attribute.declare name Pstr_eval Ast_pattern.(single_expr_payload __) (fun x -> x)
;;

let tags_attr = single_expr_attr "tags"
let time_attr = single_expr_attr "time"
let level_attr = single_expr_attr "level"

let legacy_add_extra_tag_parentheses_attr =
  Attribute.declare "legacy_tag_parentheses" Pstr_eval Ast_pattern.(pstr nil) ()
;;

let parse_level_attr decl ~extension_name_level ~loc =
  match Attribute.get level_attr decl, extension_name_level with
  | None, None -> None
  | None, Some level_arg -> Some (`Labelled (level_arg ~loc))
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

let would_log log level_arg ~loc =
  let level =
    match level_arg with
    | None -> [%expr None]
    | Some (`Optional level) -> level
    | Some (`Labelled level) -> [%expr Some [%e level]]
  in
  match log with
  | `Instance log -> [%expr Ppx_log_syntax.would_log [%e log] [%e level]]
  | `Global -> [%expr Ppx_log_syntax.Global.would_log [%e level]]
;;

let log_function ~name ~log_kind ~loc =
  match name, log_kind with
  | `Sexp, `Instance -> [%expr Ppx_log_syntax.sexp]
  | `Sexp, `Global -> [%expr Ppx_log_syntax.Global.sexp]
  | `Printf, `Instance -> [%expr Ppx_log_syntax.printf]
  | `Printf, `Global -> [%expr Ppx_log_syntax.Global.printf]
;;

let log_default ~loc = function
  | `Instance -> [%expr Ppx_log_syntax.default]
  | `Global -> [%expr Ppx_log_syntax.Global.default]
;;

module Extension_payload = struct
  (* This module represents the fact that the expression passed to the ppx may be either
     interpreted as an arg list (e.g., [%log.info log "msg" (a1 : t1) ...]) has the args
     as a list ["msg"; (a1 : t1)] vs. an pexp_apply [%log.global.info "msg" (a1 : t1)]
     ((a1 : t1) is applied to "msg").

     We can't flatten it to an arg list immediately because in the [Sexp] format, a
     pexp_apply we want to keep as a pexp_apply. *)
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

  let single_expression_or_error ~loc = function
    | Args [ (Nolabel, expr) ] | Expression expr ->
      (match expr.pexp_desc with
       | Pexp_constraint (expr, typ) ->
         let sexp_of_fn = Ppx_sexp_conv_expander.Sexp_of.core_type typ in
         Ast_builder.Default.eapply sexp_of_fn [ expr ] ~loc
       | _ -> expr)
    | (_ : t) ->
      Ast_builder.Default.pexp_extension
        ~loc
        (Location.error_extensionf
           ~loc
           "Expected exactly one unlabelled argument as payload")
  ;;
end

let function_name_and_payload format extension_payload ~loc =
  match format with
  | `Message_with_extra_tag_parentheses ->
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
    let expand e =
      Ppx_sexp_message_expander.sexp_of_labelled_exprs [ e ] ~omit_nil:false ~loc
    in
    (match Extension_payload.to_args extension_payload with
     | hd :: tl ->
       let message, tags =
         (* The first argument may be a tag, nothing, or a message. *)
         match hd with
         (* [%message "" ...] has "" ignored *)
         | Nolabel, [%expr ""] -> None, tl
         (* [%message (a : t1) (b : t2)] treats a and b both as tags *)
         | (_, [%expr ([%e? _] : [%t? _])]) as hd -> None, hd :: tl
         (* Otherwise, [%message [%e e]] treats e as a string (constant or expr). *)
         | hd -> Some hd, tl
       in
       let tags =
         match tags with
         | [] -> None
         | _ :: _ as tags ->
           let tags = List.map tags ~f:expand |> Ast_builder.Default.elist ~loc in
           Some [%expr Core.Sexp.List [%e tags]]
       in
       let sexp =
         match message, tags with
         | None, None -> [%expr Core.Sexp.List []]
         | None, Some tags -> tags
         | Some msg, None -> expand msg
         | Some msg, Some tags -> [%expr Core.Sexp.List [ [%e expand msg]; [%e tags] ]]
       in
       `Sexp, [ Nolabel, sexp ]
     | [] -> `Sexp, [ Nolabel, [%expr Core.Sexp.List []] ])
  | `Message ->
    let sexp =
      Extension_payload.to_args extension_payload
      |> Ppx_sexp_message_expander.sexp_of_labelled_exprs ~omit_nil:false ~loc
    in
    `Sexp, [ Nolabel, sexp ]
  | `Sexp ->
    (* [%log.global.sexp my_expr] assumes [my_expr] is a [Sexp.t]. [%log.global.sexp (...
       : T.t)] uses [T.sexp_of_t]. *)
    let payload = Extension_payload.single_expression_or_error extension_payload ~loc in
    `Sexp, [ Nolabel, payload ]
  | `Printf -> `Printf, Extension_payload.to_args extension_payload
  | `String ->
    let payload = Extension_payload.single_expression_or_error extension_payload ~loc in
    `Printf, [ Nolabel, [%expr "%s"]; Nolabel, payload ]
;;

let tags decl ~loc =
  let pos = Ppx_here_expander.lift_position ~loc in
  let pos_tag = [%expr [ "pos", Source_code_position.to_string [%e pos] ]] in
  let src_tags = if !log_source_position then [ pos_tag ] else [] in
  let attr_tags = Attribute.get tags_attr decl |> Option.to_list in
  match src_tags @ attr_tags with
  | [] -> [%expr None]
  | [ e ] -> [%expr Some [%e e]]
  | _ :: _ as es -> [%expr Some (List.concat [%e Ast_builder.Default.elist ~loc es])]
;;

let expand ~loc ~path:_ level format log_kind decl args =
  let loc = { loc with loc_ghost = true } in
  let tags = tags decl ~loc in
  let level = parse_level_attr decl ~extension_name_level:level ~loc in
  let time = Attribute.get time_attr decl |> Option.map ~f:(fun e -> `Optional e) in
  let format =
    match format with
    | `Message ->
      (match Attribute.get legacy_add_extra_tag_parentheses_attr decl with
       | None -> `Message
       | Some () -> `Message_with_extra_tag_parentheses)
    | format -> format
  in
  let log, extension_payload =
    match log_kind with
    | `Global -> `Global, Extension_payload.Expression args
    | `Instance ->
      Ast_pattern.(parse (pexp_apply __ __)) loc args (fun log_expr args ->
        `Instance log_expr, Extension_payload.Args args)
  in
  let name, payload_args = function_name_and_payload format extension_payload ~loc in
  let function_name = log_function ~name ~log_kind ~loc in
  let log_statement =
    let make_arg name =
      Option.map ~f:(function
        | `Optional expr -> Optional name, expr
        | `Labelled expr -> Labelled name, expr)
    in
    List.filter_opt
      [ make_arg "level" level
      ; make_arg "time" time
      ; Some (Optional "tags", tags)
      ; (match log with
         | `Global -> None
         | `Instance log -> Some (Nolabel, log))
      ]
    @ payload_args
    |> Ast_builder.Default.pexp_apply function_name ~loc
  in
  [%expr
    if [%e would_log log level ~loc] [@merlin.hide]
    then [%e log_statement]
    else [%e log_default log_kind ~loc]]
;;

let pattern =
  let open Ast_pattern in
  pstr (as__ (pstr_eval __ drop) ^:: nil)
;;

let ext name level format log =
  Extension.declare name Extension.Context.expression pattern (expand level format log)
;;

let names_and_extensions =
  let open List.Let_syntax in
  let%bind level_expr, level_str =
    [ Some (fun ~loc -> [%expr `Debug]), Some "debug"
    ; Some (fun ~loc -> [%expr `Info]), Some "info"
    ; Some (fun ~loc -> [%expr `Error]), Some "error"
    ; None, None
    ]
  in
  let%bind format, format_suffix =
    [ `Message, None; `Sexp, Some "sexp"; `Printf, Some "format"; `String, Some "string" ]
  in
  let%map log, extension_prefix = [ `Instance, "@log"; `Global, "@log.global" ] in
  let name =
    match level_str, format_suffix with
    | None, None -> extension_prefix
    | None, Some suffix -> extension_prefix ^ "." ^ suffix
    | Some level, None -> extension_prefix ^ "." ^ level
    | Some level, Some suffix -> extension_prefix ^ "." ^ level ^ "_" ^ suffix
  in
  name, ext name level_expr format log
;;

let () =
  Driver.register_transformation "log" ~extensions:(List.map names_and_extensions ~f:snd)
;;

module For_testing = struct
  let extension_names = List.map names_and_extensions ~f:fst
end
