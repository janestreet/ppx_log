open! Core
open Ppxlib

(* We're not writing a ppx, so we don't have to be careful about tracking locations when
   building ASTs. *)
let loc = Location.none

module Ast_builder = Ast_builder.Make (struct
  let loc = loc
end)

module Extension_kind = struct
  type t =
    | Message (* E.g., [%log.global.info], [%log.global] *)
    | Sexp (* E.g., [%log.global.info_sexp], [%log.global.sexp] *)
    | Format (* E.g., [%log.global.info_format], [%log.global.format] *)
    | String (* E.g., [%log.global.info_string], [%log.global.string] *)
  [@@deriving enumerate, sexp_of, variants]

  let to_suffix separator = function
    | Message -> ""
    | Sexp -> separator ^ "sexp"
    | Format -> separator ^ "format"
    | String -> separator ^ "string"
  ;;
end

module Level = struct
  type t =
    [ `Info
    | `Debug
    | `Error
    ]
  [@@deriving enumerate, sexp_of]

  let to_string = function
    | `Info -> "info"
    | `Debug -> "debug"
    | `Error -> "error"
  ;;
end

module Labelled_arg = struct
  type t = expression * [ `Mandatory | `Optional ]

  let to_attribute name expr =
    Ast_builder.(attribute ~name:(Located.mk name) ~payload:(PStr [ pstr_eval expr [] ]))
  ;;

  (* ~name:expr -> [@@name (Some expr)]
     ?name:expr -> [@@name expr] *)
  let to_optional_attribute ((expr, label_kind) : t) ~name =
    (match label_kind with
     | `Optional -> expr
     | `Mandatory -> [%expr Some [%e expr]])
    |> to_attribute name
  ;;

  let to_mandatory_attribute ((expr, label_kind) : t) ~default ~name =
    (match label_kind with
     | `Optional -> [%expr Option.value [%e expr] ~default:[%e default]]
     | `Mandatory -> expr)
    |> to_attribute name
  ;;
end

module Callsite_level = struct
  include struct
    type 'arg t =
      [ `Fixed of Level.t
      | `Variable of 'arg
      | `Unspecified
      ]
    [@@deriving enumerate, sexp_of]
  end

  let extension_name (type arg) t kind ~is_global =
    let global_str =
      match is_global with
      | `Global -> ".global"
      | `Instance -> ""
    in
    match t with
    | `Fixed level ->
      "log" ^ global_str ^ "." ^ Level.to_string level ^ Extension_kind.to_suffix "_" kind
    | `Variable (_ : arg) | `Unspecified ->
      "log" ^ global_str ^ Extension_kind.to_suffix "." kind
  ;;

  let%expect_test "extension names" =
    List.iter Extension_kind.all ~f:(fun kind ->
      List.iter (all [ () ]) ~f:(fun t ->
        List.iter [ `Instance; `Global ] ~f:(fun is_global ->
          print_s
            [%sexp
              (t : _ t)
              , (kind : Extension_kind.t)
              , (extension_name t kind ~is_global : string)])));
    [%expect
      {|
      ((Fixed Info) Message log.info)
      ((Fixed Info) Message log.global.info)
      ((Fixed Debug) Message log.debug)
      ((Fixed Debug) Message log.global.debug)
      ((Fixed Error) Message log.error)
      ((Fixed Error) Message log.global.error)
      ((Variable _) Message log)
      ((Variable _) Message log.global)
      (Unspecified Message log)
      (Unspecified Message log.global)
      ((Fixed Info) Sexp log.info_sexp)
      ((Fixed Info) Sexp log.global.info_sexp)
      ((Fixed Debug) Sexp log.debug_sexp)
      ((Fixed Debug) Sexp log.global.debug_sexp)
      ((Fixed Error) Sexp log.error_sexp)
      ((Fixed Error) Sexp log.global.error_sexp)
      ((Variable _) Sexp log.sexp)
      ((Variable _) Sexp log.global.sexp)
      (Unspecified Sexp log.sexp)
      (Unspecified Sexp log.global.sexp)
      ((Fixed Info) Format log.info_format)
      ((Fixed Info) Format log.global.info_format)
      ((Fixed Debug) Format log.debug_format)
      ((Fixed Debug) Format log.global.debug_format)
      ((Fixed Error) Format log.error_format)
      ((Fixed Error) Format log.global.error_format)
      ((Variable _) Format log.format)
      ((Variable _) Format log.global.format)
      (Unspecified Format log.format)
      (Unspecified Format log.global.format)
      ((Fixed Info) String log.info_string)
      ((Fixed Info) String log.global.info_string)
      ((Fixed Debug) String log.debug_string)
      ((Fixed Debug) String log.global.debug_string)
      ((Fixed Error) String log.error_string)
      ((Fixed Error) String log.global.error_string)
      ((Variable _) String log.string)
      ((Variable _) String log.global.string)
      (Unspecified String log.string)
      (Unspecified String log.global.string)
      |}]
  ;;

  type nonrec t = Labelled_arg.t t

  let extension_attribute t =
    match t with
    | `Variable level -> Some (Labelled_arg.to_optional_attribute level ~name:"level")
    | `Fixed (`Debug | `Error | `Info) | `Unspecified -> None
  ;;
end

module Callsite_kind = struct
  type t =
    | Sexp
    | Printf
    | String
  [@@deriving enumerate, sexp_of]

  let all =
    let open List.Let_syntax in
    (let%bind level, level_str =
       (None, "raw") :: (Level.all |> List.map ~f:(fun l -> Some l, Level.to_string l))
     in
     let%map t, suffix = [ Sexp, "_s"; Printf, "" ] in
     level_str ^ suffix, (level, t))
    @ [ "sexp", (None, Sexp); "printf", (None, Printf); "string", (None, String) ]
  ;;

  let%expect_test "all callsite kinds" =
    List.iter all ~f:(fun kind -> print_s [%sexp (kind : string * (Level.t option * t))]);
    [%expect
      {|
      (raw_s (() Sexp))
      (raw (() Printf))
      (info_s ((Info) Sexp))
      (info ((Info) Printf))
      (debug_s ((Debug) Sexp))
      (debug ((Debug) Printf))
      (error_s ((Error) Sexp))
      (error ((Error) Printf))
      (sexp (() Sexp))
      (printf (() Printf))
      (string (() String))
      |}]
  ;;
end

module Callsite = struct
  type t = Refactor_syntax.Callsite.t

  let sexp_of_t (t : t) =
    let pos = (Refactor_syntax.Callsite.loc t).loc_start in
    let expr =
      Refactor_syntax.Callsite.to_expression t |> Pprintast.string_of_expression
    in
    [%sexp { loc = [%string {|%{pos.pos_fname}:%{pos.pos_lnum#Int}|}]; expr : string }]
  ;;

  let replace t expr editor =
    Refactor_editor.replace_code_keeping_comments_exn
      editor
      ~prefix:"("
      ~suffix:[%string {|(%{expr}))|}]
      ~loc:(Refactor_syntax.Callsite.loc t)
  ;;

  let add_legacy_alert t editor ~cr =
    let add () =
      let expr =
        Ast_builder.pexp_apply
          [%expr [%e Refactor_syntax.Callsite.fn t] [@alert "-legacy"]]
          (Refactor_syntax.Callsite.args t)
      in
      Refactor_editor.replace_code_keeping_comments_exn
        editor
        ~prefix:"("
        ~suffix:[%string {|(%{Pprintast.string_of_expression expr}) )|}]
        ~loc:(Refactor_syntax.Callsite.loc t)
    in
    (* For now, we're not adding legacy alerts until we've mostly moved people over
       already. *)
    ignore (add : unit -> unit)
  ;;
end

module Log_callsite = struct
  type t =
    { time : Labelled_arg.t option
    ; tags : Labelled_arg.t option
    ; level : Callsite_level.t
    ; positional_args : expression list
    }

  let extension_attributes t =
    [ Option.map
        t.tags
        ~f:(Labelled_arg.to_mandatory_attribute ~name:"tags" ~default:[%expr []])
    ; Option.map t.time ~f:(Labelled_arg.to_optional_attribute ~name:"time")
    ; Callsite_level.extension_attribute t.level
    ]
    |> List.filter_opt
  ;;

  let create_exn (callsite : Refactor_syntax.Callsite.t) ~level_from_binding =
    let time, tags, level_from_args, positional_args =
      Refactor_syntax.Callsite.args callsite
      |> List.fold
           ~init:(None, None, None, [])
           ~f:(fun (time, tags, level, positional_args) (label, expr) ->
           let arg =
             match label with
             | Nolabel -> None
             | Labelled label -> Some (label, `Mandatory)
             | Optional label -> Some (label, `Optional)
           in
           match arg with
           | None -> time, tags, level, expr :: positional_args
           | Some (name, labelled) ->
             let arg = Some (expr, labelled) in
             (match name with
              | "time" when Option.is_none time -> arg, tags, level, positional_args
              | "tags" when Option.is_none tags -> time, arg, level, positional_args
              | "level" when Option.is_none level -> time, tags, arg, positional_args
              | _ -> raise_s [%message "Unexpected arg" (callsite : Callsite.t)]))
    in
    let positional_args = List.rev positional_args in
    let level =
      match level_from_binding, level_from_args with
      | Some level, None -> `Fixed level
      | Some _, Some _ ->
        raise_s
          [%message
            "callsite has both level implied by function name and a level argument"
              (callsite : Callsite.t)]
      | None, Some level ->
        (match level with
         | [%expr `Debug], `Mandatory | [%expr Some `Debug], `Optional -> `Fixed `Debug
         | [%expr `Info], `Mandatory | [%expr Some `Info], `Optional -> `Fixed `Info
         | [%expr `Error], `Mandatory | [%expr Some `Error], `Optional -> `Fixed `Error
         | level -> `Variable level)
      | None, None -> `Unspecified
    in
    { time; tags; positional_args; level }
  ;;
end

let sexp_of_arg_label = Ast_traverse.sexp_of#arg_label
let sexp_of_expression = Ast_traverse.sexp_of#expression

(* In the extension AST, one annoying thing is that nodes that are spiritually the same
   are represented differently. E.g., [%log "hello"] has "hello" as a single node. But
   [%log "hello" (i : int)] is a [Pexp_apply] of the string "hello" to following
   arguments. What's even worse is that [%log my_log "hello" (i : int)] is now my_log,
   applied with 2 arguments, hello and (i : int). So the transformation is quite hairy
   when a item is added to the beginning of the list. This module is intended to make that
   operation easy.
*)

module Arg_list = struct
  type t =
    { message : expression
    ; tags : (arg_label * expression) list
    }
  [@@deriving sexp_of]

  (* [~my_var:(my_var : my_type)] is not automatically changed to [(my_var : my_type)]),
     so do it here. (note, ~my_var:my_var /is/ automatically changed to ~my_var.) *)
  let apply_label_punning_on_constraint (label, e) =
    let can_remove_label =
      match label with
      | Nolabel | Optional (_ : string) -> false
      | Labelled label ->
        (match e.pexp_desc with
         | Pexp_constraint
             ( { pexp_desc = Pexp_ident { txt = Lident label_in_expression; loc = _ }; _ }
             , (_ : core_type) ) -> String.(label = label_in_expression)
         | _ -> false)
    in
    if can_remove_label then Nolabel, e else label, e
  ;;

  let to_expression { message; tags } ~should_remove_empty_string_args =
    match tags with
    | [] -> message
    | _ :: _ as tags ->
      let tags = List.map tags ~f:apply_label_punning_on_constraint in
      let tags =
        if should_remove_empty_string_args
        then
          List.filter_map tags ~f:(function
            | Nolabel, [%expr ""] -> None
            | e -> Some e)
        else tags
      in
      (* We don't use [Ast_builder.pexp_apply] since if the first element is itself a fn
         application, [Ast_builder] will join it with the other tags. *)
      { message with pexp_desc = Pexp_apply (message, tags) }
  ;;

  let prepend_arg { message; tags } arg =
    { message = arg; tags = (Nolabel, message) :: tags }
  ;;

  let create message tags = { message; tags }

  let create_unlabelled message tags =
    create message (List.map tags ~f:(fun tag -> Nolabel, tag))
  ;;
end

let parse_sexp_record_args args =
  (* [%sexp ... { f1 : t1; f2 : t2; ... }] -> [%log ... (f1 : t1) (f2 : t2) ...] *)
  List.map args ~f:(fun (id, field_expr) ->
    let name = String.concat ~sep:"." (Longident.flatten_exn id.txt) in
    match field_expr with
    | ([%expr ([%e? _] : [%t? _])] | { pexp_desc = Pexp_constant _; _ }) as e ->
      Ok (Labelled name, e)
    | _ -> Error `Unexpected_sexp_tag_arg)
  |> Result.all
;;

let parse_sexp_tuple_args args =
  List.map args ~f:(function
    (* [%sexp ..., ~~(x : int), ...] -> [%log ... (x : int) ...] *)
    | [%expr ~~[%e? labelled_e]] -> Ok (Nolabel, labelled_e)
    (* [%sexp ..., (x : int), ...] -> [%log ... ~_:(x : int) ...] *)
    | ([%expr ([%e? _] : [%t? _])] | { pexp_desc = Pexp_constant _; _ }) as e ->
      Ok (Labelled "_", e)
    | _ -> Error `Unexpected_sexp_tag_arg)
  |> Result.all
;;

let parse_sexp_callsite_args ~callsite_desc = function
  | [%expr [%message [%e? message]]] ->
    (match message.pexp_desc with
     | Pexp_apply (expr, args) -> Ok (Arg_list.create expr args, None)
     | _ -> Ok (Arg_list.create message [], None))
  | [%expr [%sexp [%e? sexp_arg]]] ->
    let nonstandard_sexp_arg () =
      (* Log.Global.sexp [%sexp ...] for a more complex expression in [%sexp] becomes
         [%log.global.sexp [%sexp ...]]. This is expected to be rare, and discouraged
         anyway, so the additional syntax I think is fine. *)
      print_s
        [%message
          "Encountered nonstandard [%sexp] expression as args to log_s callsite."
            ~_:(callsite_desc : Sexp.t)];
      Error (`Use_sexp_extension [%expr [%sexp [%e sexp_arg]]])
    in
    (match sexp_arg.pexp_desc with
     (* Log.Global.sexp [%sexp "literal"] -> [%log "literal"]*)
     | Pexp_constant _ -> Ok (Arg_list.create sexp_arg [], None)
     (* Log.Global.sexp [%sexp (fn arg : string)] -> [%log.global.sexp (fn arg : string)]

        Note you can't make it [%log.global (make_string arg)], as in the case below,
        because an [apply] is treated as a list of args in the ppx (it would appear as if
        'make_string' were a string, and 'arg' were a tag). *)
     | Pexp_constraint ({ pexp_desc = Pexp_apply _; _ }, [%type: string]) ->
       Error (`Use_sexp_extension sexp_arg)
     (* Log.Global.sexp [%sexp (string_expr : string)] -> [%log.global string_expr] *)
     | Pexp_constraint (string_expr, [%type: string]) ->
       Ok (Arg_list.create string_expr [], None)
     (* [%sexp "literal", ...], [%sexp (variable : string), ...], [%sexp [%string ...], ...] *)
     | Pexp_tuple
         (( ({ pexp_desc = Pexp_constant (Pconst_string _); _ } as string_expr)
          | [%expr ([%e? string_expr] : string)]
          | ([%expr [%string [%e? _]]] as string_expr) )
         :: exprs) ->
       let tags =
         match exprs with
         | [ { pexp_desc = Pexp_record (fields, None); _ } ] ->
           (* [%sexp msg, { ... }] -> [%log msg ...]

              Note: The sexp form renders with an extra pair of parentheses, so we need to
              account for that in the log statement for backwards-compatibility. *)
           parse_sexp_record_args fields
           |> Result.map ~f:(fun tags -> tags, Some `Legacy_tag_parentheses)
         | exprs -> parse_sexp_tuple_args exprs |> Result.map ~f:(fun tags -> tags, None)
       in
       (match tags with
        | Ok (tags, maybe_extra_attr) ->
          Ok (Arg_list.create string_expr tags, maybe_extra_attr)
        | Error `Unexpected_sexp_tag_arg -> nonstandard_sexp_arg ())
     (* [%sexp (a1 : t1), ~~(a2 : t2), ...] -> [%log "" ~_:(a1 : t1) (a2 : t2) ...] *)
     | Pexp_tuple exprs ->
       (match parse_sexp_tuple_args exprs with
        | Ok args -> Ok (Arg_list.create [%expr ""] args, None)
        | Error `Unexpected_sexp_tag_arg -> nonstandard_sexp_arg ())
     (* [%sexp { a1 : t1; a2 : t2; ...}] -> [%log (a1 : t1) (a2 : t2) ...] *)
     | Pexp_record (fields, None) ->
       (match parse_sexp_record_args fields with
        | Ok args -> Ok (Arg_list.create [%expr ""] args, Some `Legacy_tag_parentheses)
        | Error `Unexpected_sexp_tag_arg -> nonstandard_sexp_arg ())
     (* Log.Global.sexp [%sexp (expr : typ)] -> [%log.sexp (expr : typ)] *)
     | Pexp_constraint (expr, typ) ->
       Error (`Use_sexp_extension [%expr ([%e expr] : [%t typ])])
     | (_ : expression_desc) -> nonstandard_sexp_arg ())
  | nonstandard_arg -> Error (`Use_sexp_extension nonstandard_arg)
;;

let num_matches fmt_string pattern =
  String.substr_index_all fmt_string ~pattern ~may_overlap:false |> List.length
;;

let num_fmt_args fmt =
  let num_matches = num_matches fmt in
  num_matches "%" - (num_matches "%%" * 2) - num_matches "%!"
;;

let maybe_use_string_extension fmt expr =
  (* A printf can be written as a straight-up string if there are no [%]s. This is
     stricter than if [num_fmt_args = 0] because things like '%!' still require a
     format-string. *)
  if num_matches fmt "%" = 0
  then Error (`Use_string_extension (Arg_list.create expr []))
  else Ok ()
;;

let parse_printf fmt_expr args ~callsite_desc =
  let open Result.Let_syntax in
  let%map fmt_expr, num_args =
    match fmt_expr with
    (* [printf "..."] *)
    | { pexp_desc = Pexp_constant (Pconst_string (fmt, _, _)); _ } ->
      let%map () = maybe_use_string_extension fmt fmt_expr in
      fmt_expr, num_fmt_args fmt
    (* [printf !"..."] *)
    | [%expr ![%e? { pexp_desc = Pexp_constant (Pconst_string (fmt, _, _)); _ } as e]] ->
      let%map () = maybe_use_string_extension fmt e in
      (* If the fmt string has no '%'s, having '!' in the output breaks, so remove it in
         that case. *)
      let num_args = num_fmt_args fmt in
      if num_args = 0 then e, 0 else fmt_expr, num_args
    | { pexp_desc = Pexp_ident { txt = ident; _ }; _ } ->
      (* The really complicated case is when the format string is non-constant. Most of
         the time, it's a string. But it seems like there are a few wrapper libraries
         that do this. I use a heuristic here by variable name to catch these. We can
         adjust manually after. *)
      let contains substring = Longident.name ident |> String.is_substring ~substring in
      if contains "format" || contains "fmt"
      then
        Error
          (`Too_few_positional_args
            [%message
              "Callsite likely passes non-constant format argument"
                ~_:(callsite_desc : Sexp.t)])
      else Ok (fmt_expr, 0)
    | _ ->
      (* Skimming thru some cases in the tree, it seems like more complex expressions are
         either strings or constructions that don't take any additional arguments. Like
         above, we can adjust afterwards. *)
      Ok (fmt_expr, 0)
  in
  let extra_args =
    List.init
      (max 0 (num_args - List.length args))
      ~f:(function
        (* Most [printf]s with an extra arg only have 1, so don't add a number there. *)
        | 0 -> "log_arg"
        | n -> sprintf "log_arg_%d" (n + 1))
  in
  let () =
    if List.length extra_args > 0
    then
      (* For good measure, double-check that [log_arg] isn't used in the printf expression
         itself (to catch the chance we accidentally shadow it). *)
      List.iter (fmt_expr :: args) ~f:(fun expr ->
        if String.is_substring (Pprintast.string_of_expression expr) ~substring:"log_arg"
        then
          raise_s
            [%message "Unexpected argument in the printf expression that is 'log_arg'"])
  in
  let extra_arglist =
    List.map extra_args ~f:(fun arg -> Ast_builder.(pexp_ident (Located.lident arg)))
  in
  let payload = Arg_list.create_unlabelled fmt_expr (args @ extra_arglist) in
  payload, extra_args
;;

let parse_callsite_exn format positional_args ~callsite_desc =
  match format, positional_args with
  | Callsite_kind.Sexp, [ single_arg ] ->
    (match parse_sexp_callsite_args single_arg ~callsite_desc with
     | Ok (arg_list, maybe_extra_attr) ->
       Extension_kind.Message, arg_list, maybe_extra_attr, []
     | Error (`Use_sexp_extension arg) -> Sexp, Arg_list.create arg [], None, [])
    |> Ok
  | Printf, hd :: tl ->
    (match parse_printf hd tl ~callsite_desc with
     | Ok (payload, extra_args) -> Ok (Extension_kind.Format, payload, None, extra_args)
     | Error (`Use_string_extension payload) -> Ok (String, payload, None, [])
     | Error (`Too_few_positional_args e) -> Error (`Too_few_positional_args e))
  | String, [ single_arg ] ->
    (Extension_kind.String, Arg_list.create single_arg [], None, []) |> Ok
  | kind, [] ->
    Error
      (`Too_few_positional_args
        [%message
          "Log invocation has no positional args"
            (kind : Callsite_kind.t)
            ~_:(callsite_desc : Sexp.t)])
  | kind, (_ :: _ :: _ as args) ->
    raise_s
      [%message
        "Log invocation has too many positional args"
          (kind : Callsite_kind.t)
          ~num_args:(List.length args : int)
          ~_:(callsite_desc : Sexp.t)]
;;

let parse_callsite_exn format positional_args ~is_global ~callsite_desc =
  let open Result.Let_syntax in
  let%bind maybe_extra_extension_arg, positional_args =
    match is_global, positional_args with
    | `Global, args -> Ok (None, args)
    | `Instance, [] ->
      Error
        (`Too_few_positional_args
          [%message
            "Non-global log statement unexpectedly has no positional args"
              (callsite_desc : Sexp.t)])
    | `Instance, log_arg :: tl -> Ok (Some log_arg, tl)
  in
  let%map format, extension_args, maybe_extra_attribute, outer_args =
    parse_callsite_exn format positional_args ~callsite_desc
  in
  let extension_args =
    match maybe_extra_extension_arg with
    | None -> extension_args
    | Some arg -> Arg_list.prepend_arg extension_args arg
  in
  format, extension_args, maybe_extra_attribute, outer_args
;;

let legacy_tag_parentheses_attr =
  Ast_builder.(attribute ~name:(Located.mk "legacy_tag_parentheses") ~payload:(PStr []))
;;

module Exported_log_module = struct
  (* Represents what the exported [Log] module from a different file (different from the
     current file) may point to. *)
  type t =
    | Global_log (* E.g., [Import.Log] may point to [Global_log] *)
    | Async_log (* E.g., [Import.Log] may point to [Async.Log] *)
    | Neither_async_nor_global_log (* E.g., [Import.Log] may point to [Concord_log] *)
  [@@deriving sexp, enumerate]

  (* If [file_editor] points to the contents for module [Module], this function determines
     if [Module.Log] refers to [Async_log], [Async_log.Global], nothing at all, or
     something else. *)
  let determine file_editor =
    let lid = Longident.parse in
    let log = lid "Log" in
    let initial_bindings =
      [ [ log, `Not_defined ]
      ; [ lid "Log_extended", `Async_log
        ; lid "Async.Log", `Async_log
        ; lid "Async_log", `Async_log
        ]
      ; [ lid "Log_extended.Global", `Global_log
        ; lid "Async.Log.Global", `Global_log
        ; lid "Async_log.Global", `Global_log
        ]
      ]
      |> List.concat
      |> Bindings.of_alist_exn
    in
    let bindings = Bindings.process_ml_file_exn initial_bindings file_editor in
    match Bindings.find bindings log with
    | None -> Some Neither_async_nor_global_log
    | Some `Async_log -> Some Async_log
    | Some `Global_log -> Some Global_log
    | Some `Not_defined -> None
  ;;
end

let bindings ~exported_log_modules =
  let lid = Longident.parse in
  let initial_bindings =
    Callsite_kind.all
    |> List.concat_map ~f:(fun (name, (level, kind)) ->
         [ lid ("Async_log.Global." ^ name), Ok (level, kind, `Global)
         ; lid ("Async_log." ^ name), Ok (level, kind, `Instance)
         ])
  in
  let shadows, async_log_aliases =
    List.partition_map exported_log_modules ~f:(fun (`Module_name module_name, export) ->
      let log_submodule = lid [%string {|%{module_name}.Log|}] in
      match (export : Exported_log_module.t) with
      | Neither_async_nor_global_log ->
        First (log_submodule, Error `Shadowed__not_actually_async_log)
      | Global_log -> Second (lid "Async_log.Global", log_submodule)
      | Async_log -> Second (lid "Async_log", log_submodule))
  in
  let bindings = shadows @ initial_bindings |> Bindings.of_alist_exn in
  List.fold async_log_aliases ~init:bindings ~f:(fun bindings (src, dst) ->
    Bindings.with_alias bindings ~src ~dst)
  |> Bindings.with_alias ~src:(lid "Async_log") ~dst:(lid "Async.Log")
  |> Bindings.with_alias ~src:(lid "Async_log") ~dst:(lid "Log_extended")
  |> Bindings.with_alias ~src:(lid "Async_log") ~dst:(lid "Kazoo_async.Log")
;;

let convert_callsites_to_ppx_log editor ~actually_transform ~initial_bindings =
  let partial_application_cr =
    "Consider using [ppx_log] directly insteading of partially applying a log function \
     in [Async_log]."
  in
  object
    inherit [_] Refactor_syntax.iter_callsites_of_bindings editor ~initial_bindings

    method! callsite_of_binding callsite (_ : longident_loc) =
      function
      | Error `Shadowed__not_actually_async_log -> ()
      | Ok (level_from_binding, format, is_global) ->
        let callsite_desc = [%sexp (callsite : Callsite.t)] in
        let log_callsite = Log_callsite.create_exn callsite ~level_from_binding in
        let attrs = Log_callsite.extension_attributes log_callsite in
        (match
           parse_callsite_exn
             format
             log_callsite.positional_args
             ~is_global
             ~callsite_desc
         with
         | Ok (kind, extension_args, maybe_extra_attr, outer_args) ->
           let name = Callsite_level.extension_name log_callsite.level kind ~is_global in
           let payload =
             let should_remove_empty_string_args = Extension_kind.is_message kind in
             Arg_list.to_expression extension_args ~should_remove_empty_string_args
           in
           let attrs =
             match maybe_extra_attr with
             | None -> attrs
             | Some `Legacy_tag_parentheses -> legacy_tag_parentheses_attr :: attrs
           in
           let extension =
             Ast_builder.(
               pexp_extension (Located.mk name, PStr [ pstr_eval payload attrs ]))
           in
           let extension =
             List.fold (List.rev outer_args) ~init:extension ~f:(fun extension arg ->
               Ast_builder.pexp_fun
                 Nolabel
                 None
                 Ast_builder.(ppat_var (Located.mk arg))
                 extension)
             |> Pprintast.string_of_expression
           in
           let has_async_as_prefix =
             match
               Or_error.try_with (fun () ->
                 Refactor_syntax.Callsite.fn callsite
                 |> Pprintast.string_of_expression
                 |> Longident.parse
                 |> Longident.flatten_exn)
             with
             | Ok (("Async" | "Async_log") :: _) -> true
             | Error e ->
               print_s [%message "Warning: callsite function nonstandard" (e : Error.t)];
               false
             | Ok (_ : string list) -> false
           in
           let extension =
             (* I'd do the replacement w/ metaquot, but [Pprintast.string_of_expression
                [%expr Module.(X)]] becomes [let open Module in X] *)
             if has_async_as_prefix
             then [%string {|Async_log.Ppx_log_syntax.(%{extension})|}]
             else extension
           in
           if actually_transform then Callsite.replace callsite extension editor
         | Error (`Nonstandard_format_string e) ->
           print_s e;
           if actually_transform
           then
             Callsite.add_legacy_alert
               callsite
               editor
               ~cr:"Consider switching to ppx log."
         | Error (`Too_few_positional_args e) ->
           if not am_running_test then print_s e;
           if actually_transform
           then Callsite.add_legacy_alert callsite editor ~cr:partial_application_cr)

    method! argument_as_callsite_of_binding callsite (_ : longident_loc) _ =
      print_s
        [%message
          "Callsite was passed as argument to other function" ~_:(callsite : Callsite.t)];
      if actually_transform
      then Callsite.add_legacy_alert callsite editor ~cr:partial_application_cr
  end
;;

let has_alert_attribute expression =
  List.exists expression.pexp_attributes ~f:(function
    | { attr_name = { txt = "alert"; loc = _ }
      ; attr_payload = PStr [%str "-use_ppx_log"]
      ; attr_loc = _
      } -> true
    | _ -> false)
;;

let add_alert_to_ml_file editor ~actually_transform ~initial_bindings =
  object
    inherit
      [_] Refactor_syntax.iter_with_bindings editor ~initial_bindings ~interpret_as:Name as super

    (* There have been some false-positive matches where an alert was added to something
       in an extension or attribute. Hence the disabling of the iteration down some of
       these AST nodes. *)

    method! extension (label, payload) =
      match label.txt with
      | "map" | "bind" -> super#extension (label, payload)
      | "sexp" | "sexp_of" | "message" -> ()
      | s ->
        if String.is_prefix s ~prefix:"test" then () else super#extension (label, payload)

    method! attribute (_ : attribute) = ()
    method! type_declaration (_ : type_declaration) = ()
    method! expression e = if not (has_alert_attribute e) then super#expression e

    method! binding { loc; txt = _ } namespace instance =
      match namespace, instance with
      | ( Value
        , Ok ((_ : Level.t option), (_ : Callsite_kind.t), (_ : [> `Global | `Instance ]))
        ) ->
        print_endline
          [%string
            {|Found binding: %{loc.loc_start.pos_fname}:%{loc.loc_start.pos_lnum#Int}|}];
        if actually_transform
        then (
          let contents = Refactor_editor.contents_at_exn editor ~loc in
          Refactor_editor.replace_code_keeping_comments_exn
            editor
            ~prefix:[%string {|((%{contents})|}]
            ~suffix:{|[@alert "-use_ppx_log"])|}
            ~loc)
      | _, (Error `Shadowed__not_actually_async_log | Ok _) -> ()
  end
;;

let%test_module "" =
  (module struct
    let test_alert file_contents =
      let initial_bindings = bindings ~exported_log_modules:[] in
      Or_error.try_with (fun () ->
        Refactor_syntax.transform_exn
          (add_alert_to_ml_file ~actually_transform:true ~initial_bindings)
          ~filename:(File_path.of_string "file.ml")
          ~file_contents)
    ;;

    let%expect_test "test" =
      let result =
        test_alert
          {|
          open! Async
          let l = Log.Global.printf
          let () = Log.Global.info_s [%message error_str]
          let () = (Log.Global.info_s [@alert "-use_ppx_log"]) [%message error_str]
          let () = Async.Log.Global.(print_s [%sexp (a : string)])
          let () =
            match%map Deferred.unit with
            | () -> Error.sexp_of_t e |> Log.Global.error_s ~tags:[ "line", line ]
          ;;

          let () =
            let%map () = Deferrred.unit in
            Log.Global.info_s [%message error_str]
          ;;

          open! Log.Global
          type t = { t : string } [@@deriving sexp]

          let () = [%test_eq: string] ("a" : string) "b"

          module Log = struct
            let () = Log.Global.info_s [%message error_str]
          end

   |}
        |> ok_exn
        |> String.substr_replace_all ~pattern:"\n  " ~with_:"\n"
      in
      [%expect
        {|
        Found binding: file.ml:3
        Found binding: file.ml:4
        Found binding: file.ml:9
        Found binding: file.ml:14
        Found binding: file.ml:23
        |}];
      print_endline result;
      [%expect
        {|
        open! Async
        let l = ((Log.Global.printf)[@alert "-use_ppx_log"])
        let () = ((Log.Global.info_s)[@alert "-use_ppx_log"]) [%message error_str]
        let () = (Log.Global.info_s [@alert "-use_ppx_log"]) [%message error_str]
        let () = Async.Log.Global.(print_s [%sexp (a : string)])
        let () =
          match%map Deferred.unit with
          | () -> Error.sexp_of_t e |> ((Log.Global.error_s)[@alert "-use_ppx_log"]) ~tags:[ "line", line ]
        ;;

        let () =
          let%map () = Deferrred.unit in
          ((Log.Global.info_s)[@alert "-use_ppx_log"]) [%message error_str]
        ;;

        open! Log.Global
        type t = { t : string } [@@deriving sexp]

        let () = [%test_eq: string] ("a" : string) "b"

        module Log = struct
          let () = ((Log.Global.info_s)[@alert "-use_ppx_log"]) [%message error_str]
        end
        |}]
    ;;

    let test import_log file_contents =
      let initial_bindings =
        match import_log with
        | None -> bindings ~exported_log_modules:[]
        | Some import_log ->
          bindings ~exported_log_modules:[ `Module_name "Import", import_log ]
      in
      Or_error.try_with (fun () ->
        Refactor_syntax.transform_exn
          (convert_callsites_to_ppx_log ~actually_transform:true ~initial_bindings)
          ~filename:(File_path.of_string "file.ml")
          ~file_contents)
    ;;

    let%expect_test "ppx log is used iff async log is in scope, not shadowed by import" =
      List.iter [%all: Exported_log_module.t option] ~f:(fun import_log ->
        printf
          !"\nIf [Import.Log] is %{Sexp} then\n"
          [%sexp (import_log : Exported_log_module.t option)];
        List.iter [ "open Import"; "open Async open Import" ] ~f:(fun opens ->
          List.iter
            [ {|let () = Log.Global.info_s [%message ""]|}
            ; {|let () = Log.info_s log [%message ""]|}
            ; {|let () = Log.info_s [%message ""]|}
            ]
            ~f:(fun statement ->
            let contents = opens ^ " " ^ statement in
            match test import_log contents with
            | Ok result ->
              if String.(contents <> result)
              then printf "- changed:  -[%s]\n  into:     +[%s]\n\n" contents result
              else printf "- unchanged: [%s]\n" contents
            | Error (_ : Error.t) -> printf "- error:    ![%s]\n" contents)));
      [%expect
        {|
        If [Import.Log] is () then
        - unchanged: [open Import let () = Log.Global.info_s [%message ""]]
        - unchanged: [open Import let () = Log.info_s log [%message ""]]
        - unchanged: [open Import let () = Log.info_s [%message ""]]
        - changed:  -[open Async open Import let () = Log.Global.info_s [%message ""]]
          into:     +[open Async open Import let () = (([%log.global.info ""]))]

        - changed:  -[open Async open Import let () = Log.info_s log [%message ""]]
          into:     +[open Async open Import let () = (([%log.info log ]))]

        - unchanged: [open Async open Import let () = Log.info_s [%message ""]]

        If [Import.Log] is (Global_log) then
        - unchanged: [open Import let () = Log.Global.info_s [%message ""]]
        - error:    ![open Import let () = Log.info_s log [%message ""]]
        - changed:  -[open Import let () = Log.info_s [%message ""]]
          into:     +[open Import let () = (([%log.global.info ""]))]

        - unchanged: [open Async open Import let () = Log.Global.info_s [%message ""]]
        - error:    ![open Async open Import let () = Log.info_s log [%message ""]]
        - changed:  -[open Async open Import let () = Log.info_s [%message ""]]
          into:     +[open Async open Import let () = (([%log.global.info ""]))]


        If [Import.Log] is (Async_log) then
        - changed:  -[open Import let () = Log.Global.info_s [%message ""]]
          into:     +[open Import let () = (([%log.global.info ""]))]

        - changed:  -[open Import let () = Log.info_s log [%message ""]]
          into:     +[open Import let () = (([%log.info log ]))]

        - unchanged: [open Import let () = Log.info_s [%message ""]]
        - changed:  -[open Async open Import let () = Log.Global.info_s [%message ""]]
          into:     +[open Async open Import let () = (([%log.global.info ""]))]

        - changed:  -[open Async open Import let () = Log.info_s log [%message ""]]
          into:     +[open Async open Import let () = (([%log.info log ]))]

        - unchanged: [open Async open Import let () = Log.info_s [%message ""]]

        If [Import.Log] is (Neither_async_nor_global_log) then
        - unchanged: [open Import let () = Log.Global.info_s [%message ""]]
        - unchanged: [open Import let () = Log.info_s log [%message ""]]
        - unchanged: [open Import let () = Log.info_s [%message ""]]
        - unchanged: [open Async open Import let () = Log.Global.info_s [%message ""]]
        - unchanged: [open Async open Import let () = Log.info_s log [%message ""]]
        - unchanged: [open Async open Import let () = Log.info_s [%message ""]]
        |}]
    ;;

    let%expect_test "test" =
      let result =
        test
          (Some Async_log)
          {|
   open Core
   open Async

   let () =
   Log.Global.info_s [%message error_str];
   Log.Global.info_s [%message (hostname : Hostname.t) (e : Error.t) [@@tags "hello", "world"]];
   Log.Global.info_s [%message "message" ~_:(List.hd_exn errors : Error.t)];
   Log.Global.info_s [%message "message" (i : int option[@sexp.option])];
   Log.Global.info_s [%message "message" (i : int option[@sexp.omit_nil])];

   Log.Global.info_s [%sexp (e : Error.t)];
   Log.Global.info_s error_sexp;
   Log.Global.info_s [%sexp "message", (host : Hostname.t), ~~(error : Error.t)];
   Log.Global.info_s [%sexp (my_message : string), (* Test comment *)(host : Hostname.t)];
   Log.Global.info_s [%sexp (my_message : string), { host : Hostname.t }];
   Log.Global.info_s [%sexp [%string "hello %d" 1], { host : Hostname.t }];

   Log.Global.sexp ~level:`Debug ~time ~tags:my_tags [%message "hello"];
   (* Places where [level] and [time] are variable are few / generally only log wrappers. *)
   Log.Global.sexp ~level:my_level ?time ~tags:my_tags [%message "hello"];
   Log.Global.sexp ?tags:my_tags [%message "hello"];

   Log.Global.printf "hello world";
   Log.Global.printf "hello world %s" i;
   Log.Global.printf "hello world %s %% %s";

   Log.info_s (my log) [%message error_str];
   Log.info_s (my log) [%message (hostname : Hostname.t) (e : Error.t) [@@tags "hello", "world"]];
   Log.info_s (my log) [%message "message" ~_:(List.hd_exn errors : Error.t)];
   Log.info_s (my log) [%message "message" (i : int option[@sexp.option])];
   Log.info_s (my log) [%message "message" (i : int option[@sexp.omit_nil])];

   Log.info_s (my log) [%sexp (e : Error.t)];
   Log.info_s (my log) error_sexp;
   Log.info_s (my log) [%sexp "message", (host : Hostname.t), ~~(error : Error.t)];
   Log.info_s (my log) [%sexp (my_message : string), (host : Hostname.t)];
   Log.info_s (my log) [%sexp (my_message : string), { host : Hostname.t }];
   Log.info_s (my log) [%sexp ((a b) : int), (b : int)];
   Log.info_s (my log) [%sexp { a : int; b : int }];
   Log.info_s (my log) [%sexp { a = 123 }];

   do_thing ~log:(Log.Global.info_s);
   do_thing ~log:(Log.info_s (my log));
   do_thing ~log:(Log.info_s ~time:Time_ns.epoch);

   Log.sexp (my log) ~level:`Debug ~time [%message "hello" [@@tags my_tags]];
   Log.sexp (my log) ~level:my_level ?time [%message "hello" [@@tags my_tags]];
   Log.printf (my log) "hello world";
   Log.printf (my log) "hello world %s" i

   module Log = Log.Global
   let () = Log.sexp [%message "hello"]
   ;;
   |}
        |> ok_exn
        |> String.substr_replace_all ~pattern:"\n  " ~with_:"\n"
      in
      [%expect
        {|
        ("Callsite was passed as argument to other function"
         ((loc file.ml:43) (expr Log.Global.info_s)))
        |}];
      print_endline result;
      [%expect
        {|
         open Core
         open Async

         let () =
         (([%log.global.info error_str]));
         (([%log.global.info (hostname : Hostname.t) (e : Error.t)]));
         (([%log.global.info "message" ~_:(List.hd_exn errors : Error.t)]));
         (([%log.global.info "message" (i : ((int option)[@sexp.option ]))]));
         (([%log.global.info "message" (i : ((int option)[@sexp.omit_nil ]))]));

         (([%log.global.info_sexp (e : Error.t)]));
         (([%log.global.info_sexp error_sexp]));
         (([%log.global.info "message" ~_:(host : Hostname.t) (error : Error.t)]));
         ((* Test comment *)([%log.global.info my_message ~_:(host : Hostname.t)]));
         (([%log.global.info my_message (host : Hostname.t)[@@legacy_tag_parentheses ]]));
         (([%log.global.info
        ([%string "hello %d" 1]) (host : Hostname.t)[@@legacy_tag_parentheses ]]));

         (([%log.global.debug "hello"[@@tags my_tags][@@time Some time]]));
         (* Places where [level] and [time] are variable are few / generally only log wrappers. *)
         (([%log.global "hello"[@@tags my_tags][@@time time][@@level Some my_level]]));
         (([%log.global "hello"[@@tags Option.value my_tags ~default:[]]]));

         (([%log.global.string "hello world"]));
         (([%log.global.format "hello world %s" i]));
         ((fun log_arg log_arg_2 ->
        [%log.global.format "hello world %s %% %s" log_arg log_arg_2]));

         (([%log.info (my log) error_str]));
         (([%log.info (my log) (hostname : Hostname.t) (e : Error.t)]));
         (([%log.info (my log) "message" ~_:(List.hd_exn errors : Error.t)]));
         (([%log.info (my log) "message" (i : ((int option)[@sexp.option ]))]));
         (([%log.info (my log) "message" (i : ((int option)[@sexp.omit_nil ]))]));

         (([%log.info_sexp (my log) (e : Error.t)]));
         (([%log.info_sexp (my log) error_sexp]));
         (([%log.info (my log) "message" ~_:(host : Hostname.t) (error : Error.t)]));
         (([%log.info (my log) my_message ~_:(host : Hostname.t)]));
         (([%log.info
        (my log) my_message (host : Hostname.t)[@@legacy_tag_parentheses ]]));
         (([%log.info (my log) ~_:(a b : int) ~_:(b : int)]));
         (([%log.info (my log) (a : int) (b : int)[@@legacy_tag_parentheses ]]));
         (([%log.info (my log) ~a:123[@@legacy_tag_parentheses ]]));

         do_thing ~log:(Log.Global.info_s);
         do_thing ~log:(Log.info_s (my log));
         do_thing ~log:(Log.info_s ~time:Time_ns.epoch);

         (([%log.debug (my log) "hello"[@@time Some time]]));
         (([%log (my log) "hello"[@@time time][@@level Some my_level]]));
         (([%log.string (my log) "hello world"]));
         (([%log.format (my log) "hello world %s" i]))

         module Log = Log.Global
         let () = (([%log.global "hello"]))
         ;;
        |}]
    ;;
  end)
;;

open! Async

module Mode = struct
  module T = struct
    type t = [ `Strict ] [@@deriving enumerate, sexp_of]
  end

  include T

  let param =
    Roundtrippable_command_param.create_required_from_enum
      "mode"
      (module T)
      ~doc:
        "Strict mode creates slightly uglier code in a few cases in order to completely \
         preserve output format (e.g., parentheses and locations of dynamic tags). There \
         will be a loose mode that does not preserve output format, but that will come \
         after other changes to ppx_log (targetting later in 2023-11)"
  ;;
end

type t =
  { actually_transform : bool
  ; cache_dir : File_path.t
  ; mode : Mode.t
  }
[@@deriving fields ~getters ~iterators:(create, make_creator)]

let param =
  Roundtrippable_command_param.Record_builder.(
    build_for_record
      (Fields.make_creator
         ~mode:(field Mode.param)
         ~actually_transform:
           (field
              (Roundtrippable_command_param.create_required
                 "actually-transform"
                 Command.Param.Arg_type.Export.bool
                 ~to_string:Bool.to_string
                 ~doc:"BOOL Actually transform the code, vs. only parse and print errors"))
         ~cache_dir:
           (field
              (Roundtrippable_command_param.create_required
                 "shared-cache-path"
                 File_path.arg_type
                 ~to_string:File_path.to_string
                 ~doc:
                   "PATH This smash tool runs multiple workers in parallel and uses a \
                    temporary directory to save shared computations about what modules \
                    [Log]. This is the path to the temporary directory. (Usually \
                    $(mktemp -d) is fine, but this tool doesn't clean up the directory.)"))))
;;

module Cache = struct
  module Data = struct
    module Entry = struct
      type t = [ `Module_name of string ] * Exported_log_module.t [@@deriving sexp]

      let determine project filename =
        match File_path.basename filename with
        | None -> return None
        | Some basename ->
          (match String.lsplit2 (File_path.Part.to_string basename) ~on:'.' with
           | None -> return None
           | Some (module_name, "ml") ->
             let module_name = String.capitalize module_name in
             if%bind Filesystem_async.exists_exn filename
             then
               Refactor_project.find project ~filename
               >>| Exported_log_module.determine
               >>| Option.map ~f:(fun export -> `Module_name module_name, export)
             else return None
           | Some ((_ : string), (_ : string)) -> return None)
      ;;
    end

    type t = Entry.t list [@@deriving sexp]

    let files_in_directory working_dir ~matching =
      Refactor_bash.Rg.command (Refactor_bash.Rg.create () ~matching)
      |> Refactor_bash.run_for_stdout ~working_dir ~accept_nonzero_exit:[ 1 ]
      >>| String.split ~on:'\n'
      >>| List.filter ~f:(Fn.non String.is_empty)
    ;;

    let compute project dir =
      print_s [%message "Computing log module exports" (dir : File_path.t)];
      let%bind files = files_in_directory dir ~matching:(Regex {|^module Log = |}) in
      Deferred.List.filter_map files ~how:`Parallel ~f:(fun filename ->
        File_path.append dir (File_path.Relative.of_string filename)
        |> Entry.determine project)
    ;;
  end

  let sanitize_path_to_string path =
    File_path.to_string path
    |> String.map ~f:(function
         | ('a' .. 'z' | '0' .. '9' | '-' | '_') as c -> c
         | _ -> '_')
    |> File_path.Part.of_string
  ;;

  let create ~project ~cache_dir =
    Filesystem_cache.create
      ~key_to_filename:sanitize_path_to_string
      ~data_format:(module Data)
      ~compute:(Data.compute project)
      ~cache_dir
  ;;
end

let make_smash_command f ~summary =
  let find_files_based_on =
    Refactor_bash.Rg.create ~matching:(Regex {|(log|Log|L|Log_extended)\.|}) ()
  in
  let instance_impl
    { actually_transform; cache_dir; mode = `Strict }
    ~repo:_
    ~project
    ~session:_
    filename
    =
    if String.is_suffix ~suffix:".ml" (File_path.to_string filename)
    then (
      let cache = Cache.create ~project ~cache_dir in
      let%bind editor = Refactor_project.find project ~filename in
      let%map exported_log_modules =
        Filesystem_cache.fetch cache (File_path.dirname_exn filename)
      in
      let initial_bindings = bindings ~exported_log_modules in
      Refactor_editor.transform_exn editor ~f:(fun e ->
        f e ~actually_transform ~initial_bindings))
    else return ()
  in
  Refactor_parallel.single_command
    ~find_files_based_on
    ~instance_impl
    ~shared_param:param
    ~summary
    ()
;;

let convert_callsites_to_ppx_log_command =
  make_smash_command
    convert_callsites_to_ppx_log
    ~summary:"Replaces Log[.Global] logging functions with ppx_log equivalents"
;;

let add_alert_to_ml_file_command =
  make_smash_command
    add_alert_to_ml_file
    ~summary:"Add alerts to direct async log functions"
;;

let add_alert_to_jbuild_command =
  Command.async
    ~summary:"add a alert disabling clause to a jbuild"
    (let%map_open.Command file = anon ("FILE" %: string) in
     fun () ->
       let%bind file_contents = Reader.file_contents file in
       let contents =
         Refactor_jbuild.add_flags
           ~file_contents
           ~missing_flags:[ "-alert"; "-use_ppx_log" ]
           ()
       in
       Writer.save file ~contents)
;;

let command =
  Command.group
    ~summary:"tools for smashing the tree related to async log"
    [ "smash", convert_callsites_to_ppx_log_command
    ; ( "alerts"
      , Command.group
          ~summary:"tools for adding alerts for async log"
          [ "add-to-jbuild", add_alert_to_jbuild_command
          ; "add-to-ml", add_alert_to_ml_file_command
          ] )
    ]
;;
