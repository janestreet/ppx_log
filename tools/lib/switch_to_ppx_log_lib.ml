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
  [@@deriving enumerate, sexp_of]

  let to_suffix separator = function
    | Message -> ""
    | Sexp -> separator ^ "sexp"
    | Format -> separator ^ "format"
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

  (* ~name:expr -> [@@name (Some expr)]
     ?name:expr -> [@@name expr] *)
  let to_attribute ((expr, label_kind) : t) ~name =
    let open Ast_builder in
    let expr =
      match label_kind with
      | `Optional -> expr
      | `Mandatory -> [%expr Some [%e expr]]
    in
    attribute ~name:(Located.mk name) ~payload:(PStr [ pstr_eval expr [] ])
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
    let global_str = if is_global then ".global" else "" in
    match t with
    | `Fixed level ->
      "log" ^ global_str ^ "." ^ Level.to_string level ^ Extension_kind.to_suffix "_" kind
    | `Variable (_ : arg) | `Unspecified ->
      "log" ^ global_str ^ Extension_kind.to_suffix "." kind
  ;;

  let%expect_test "extension names" =
    List.iter Extension_kind.all ~f:(fun kind ->
      List.iter (all [ () ]) ~f:(fun t ->
        List.iter Bool.all ~f:(fun is_global ->
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
      (Unspecified Format log.global.format) |}]
  ;;

  type nonrec t = Labelled_arg.t t

  let extension_attribute t =
    match t with
    | `Variable level -> Some (Labelled_arg.to_attribute level ~name:"level")
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
      (string (() String)) |}]
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
    Refactor_editor.replace
      editor
      ~with_:(Pprintast.string_of_expression expr)
      ~loc:(Refactor_syntax.Callsite.loc t)
  ;;

  let add_legacy_alert t editor ~cr =
    let expr =
      Ast_builder.pexp_apply
        [%expr [%e Refactor_syntax.Callsite.fn t] [@alert "-legacy"]]
        (Refactor_syntax.Callsite.args t)
    in
    Refactor_editor.replace
      editor
      ~with_:[%string {|(%{Pprintast.string_of_expression expr}) |}]
      ~loc:(Refactor_syntax.Callsite.loc t)
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
    [ Option.map t.tags ~f:(Labelled_arg.to_attribute ~name:"tags")
    ; Option.map t.time ~f:(Labelled_arg.to_attribute ~name:"time")
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

  let to_expression { message; tags } =
    match tags with
    | [] -> message
    | _ :: _ as tags ->
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
  (* [%sexp ... { f1 : t1; f2 : t2; ... }] -> [%log ... (f1 : t1) (f2 : t2) ...]

     Note: The sexp form renders with an extra pair of parentheses, so we need to account
     for that in the log statement for backwards-compatibility. *)
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
     | Pexp_apply (expr, args) -> Ok (Arg_list.create expr args)
     | _ -> Ok (Arg_list.create message []))
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
     | Pexp_constant _ -> Ok (Arg_list.create sexp_arg [])
     (* Log.Global.sexp [%sexp (string_var : string)] -> [%log string_var] *)
     | Pexp_constraint (string_var, [%type: string]) -> Ok (Arg_list.create string_var [])
     (* [%sexp "literal", ...], [%sexp (variable : string), ...], [%sexp [%string ...], ...] *)
     | Pexp_tuple
         (( ({ pexp_desc = Pexp_constant (Pconst_string _); _ } as string_expr)
          | [%expr ([%e? string_expr] : string)]
          | ([%expr [%string [%e? _]]] as string_expr) )
         :: exprs) ->
       let tags =
         match exprs with
         | [ { pexp_desc = Pexp_record (fields, None); _ } ] ->
           parse_sexp_record_args fields
         | exprs -> parse_sexp_tuple_args exprs
       in
       (match tags with
        | Ok tags -> Ok (Arg_list.create string_expr tags)
        | Error `Unexpected_sexp_tag_arg -> nonstandard_sexp_arg ())
     (* [%sexp (a1 : t1), ~~(a2 : t2), ...] -> [%log "" ~_:(a1 : t1) (a2 : t2) ...] *)
     | Pexp_tuple exprs ->
       (match parse_sexp_tuple_args exprs with
        | Ok args -> Ok (Arg_list.create [%expr ""] args)
        | Error `Unexpected_sexp_tag_arg -> nonstandard_sexp_arg ())
     (* [%sexp { a1 : t1; a2 : t2; ...}] -> [%log (a1 : t1) (a2 : t2) ...] *)
     | Pexp_record (fields, None) ->
       (match parse_sexp_record_args fields with
        | Ok args -> Ok (Arg_list.create [%expr ""] args)
        | Error `Unexpected_sexp_tag_arg -> nonstandard_sexp_arg ())
     (* Log.Global.sexp [%sexp (expr : typ)] -> [%log.sexp (expr : typ)] *)
     | Pexp_constraint (expr, typ) ->
       Error (`Use_sexp_extension [%expr ([%e expr] : [%t typ])])
     | (_ : expression_desc) -> nonstandard_sexp_arg ())
  | nonstandard_arg -> Error (`Use_sexp_extension nonstandard_arg)
;;

let parse_callsite_exn format positional_args ~callsite_desc =
  match format, positional_args with
  | Callsite_kind.Sexp, [ single_arg ] ->
    (match parse_sexp_callsite_args single_arg ~callsite_desc with
     | Ok arg_list -> Extension_kind.Message, arg_list
     | Error (`Use_sexp_extension arg) -> Sexp, Arg_list.create arg [])
    |> Ok
  | Printf, hd :: tl -> (Format, Arg_list.create_unlabelled hd tl) |> Ok
  | String, [ single_arg ] ->
    (Extension_kind.Message, Arg_list.create single_arg []) |> Ok
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
    | true, args -> Ok (None, args)
    | false, [] ->
      Error
        (`Too_few_positional_args
          [%message
            "Non-global log statement unexpectedly has no positional args"
              (callsite_desc : Sexp.t)])
    | false, log_arg :: tl -> Ok (Some log_arg, tl)
  in
  let%map format, extension_args =
    parse_callsite_exn format positional_args ~callsite_desc
  in
  let extension_args =
    match maybe_extra_extension_arg with
    | None -> extension_args
    | Some arg -> Arg_list.prepend_arg extension_args arg
  in
  format, extension_args
;;

let parse editor ~actually_transform =
  let initial_bindings =
    Callsite_kind.all
    |> List.concat_map ~f:(fun (name, (level, kind)) ->
         [ Longident.parse ("Async.Log.Global." ^ name), (level, kind, true)
         ; Longident.parse ("Async.Log." ^ name), (level, kind, false)
         ])
    |> Refactor_syntax.Bindings.of_alist_exn
  in
  let partial_application_cr =
    "Consider passing a [Log.t] directly to use with [ppx_log], instead of passing one \
     of the functions."
  in
  object
    inherit [_] Refactor_syntax.iter_callsites_of_bindings editor ~initial_bindings

    method! callsite_of_binding
      callsite
      (_ : longident_loc)
      (level_from_binding, format, is_global) =
      let callsite_desc = [%sexp (callsite : Callsite.t)] in
      let log_callsite = Log_callsite.create_exn callsite ~level_from_binding in
      let attrs = Log_callsite.extension_attributes log_callsite in
      match
        parse_callsite_exn format log_callsite.positional_args ~is_global ~callsite_desc
      with
      | Ok (kind, extension_args) ->
        let name = Callsite_level.extension_name log_callsite.level kind ~is_global in
        let payload = Arg_list.to_expression extension_args in
        let extension =
          Ast_builder.(pexp_extension (Located.mk name, PStr [ pstr_eval payload attrs ]))
        in
        if actually_transform then Callsite.replace callsite extension editor
      | Error (`Too_few_positional_args e) ->
        print_s e;
        if actually_transform
        then Callsite.add_legacy_alert callsite editor ~cr:partial_application_cr

    method! argument_as_callsite_of_binding callsite (_ : longident_loc) _ =
      print_s
        [%message
          "Callsite was passed as argument to other function" ~_:(callsite : Callsite.t)];
      if actually_transform
      then Callsite.add_legacy_alert callsite editor ~cr:partial_application_cr
  end
;;

let%expect_test "test" =
  let result =
    Refactor_syntax.transform_exn
      (parse ~actually_transform:true)
      ~filename:(File_path.of_string "file.ml")
      ~file_contents:
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
  Log.Global.info_s [%sexp (my_message : string), (host : Hostname.t)];
  Log.Global.info_s [%sexp (my_message : string), { host : Hostname.t }];
  Log.Global.info_s [%sexp [%string "hello %d" 1], { host : Hostname.t }];

  Log.Global.sexp ~level:`Debug ~time ~tags:my_tags [%message "hello"];
  (* Places where [level] and [time] are variable are few / generally only log wrappers. *)
  Log.Global.sexp ~level:my_level ?time ~tags:my_tags [%message "hello"];

  Log.Global.printf "hello world";
  Log.Global.printf "hello world %s" i;

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
;;
|}
    |> String.substr_replace_all ~pattern:"\n  " ~with_:"\n"
  in
  [%expect
    {|
    ("Callsite was passed as argument to other function"
     ((loc file.ml:41) (expr Log.Global.info_s)))
    ("Log invocation has no positional args" (kind Sexp)
     ((loc file.ml:42) (expr "Log.info_s (my log)")))
    ("Non-global log statement unexpectedly has no positional args"
     (callsite_desc ((loc file.ml:43) (expr "Log.info_s ~time:Time_ns.epoch")))) |}];
  print_endline result;
  [%expect
    {|
    open Core
    open Async

    let () =
    [%log.global.info error_str];
    [%log.global.info (hostname : Hostname.t) (e : Error.t)];
    [%log.global.info "message" ~_:(List.hd_exn errors : Error.t)];
    [%log.global.info "message" (i : ((int option)[@sexp.option ]))];
    [%log.global.info "message" (i : ((int option)[@sexp.omit_nil ]))];

    [%log.global.info_sexp (e : Error.t)];
    [%log.global.info_sexp error_sexp];
    [%log.global.info "message" ~_:(host : Hostname.t) (error : Error.t)];
    [%log.global.info my_message ~_:(host : Hostname.t)];
    [%log.global.info my_message ~host:(host : Hostname.t)];
    [%log.global.info ([%string "hello %d" 1]) ~host:(host : Hostname.t)];

    [%log.global.debug "hello"[@@tags Some my_tags][@@time Some time]];
    (* Places where [level] and [time] are variable are few / generally only log wrappers. *)
    [%log.global
    "hello"[@@tags Some my_tags][@@time time][@@level Some my_level]];

    [%log.global.format "hello world"];
    [%log.global.format "hello world %s" i];

    [%log.info (my log) error_str];
    [%log.info (my log) (hostname : Hostname.t) (e : Error.t)];
    [%log.info (my log) "message" ~_:(List.hd_exn errors : Error.t)];
    [%log.info (my log) "message" (i : ((int option)[@sexp.option ]))];
    [%log.info (my log) "message" (i : ((int option)[@sexp.omit_nil ]))];

    [%log.info_sexp (my log) (e : Error.t)];
    [%log.info_sexp (my log) error_sexp];
    [%log.info (my log) "message" ~_:(host : Hostname.t) (error : Error.t)];
    [%log.info (my log) my_message ~_:(host : Hostname.t)];
    [%log.info (my log) my_message ~host:(host : Hostname.t)];
    [%log.info (my log) "" ~_:(a b : int) ~_:(b : int)];
    [%log.info (my log) "" ~a:(a : int) ~b:(b : int)];
    [%log.info (my log) "" ~a:123];

    do_thing ~log:(((Log.Global.info_s)[@alert "-legacy"])) ;
    do_thing ~log:(((Log.info_s)[@alert "-legacy"]) (my log)) ;
    do_thing ~log:(((Log.info_s)[@alert "-legacy"]) ~time:Time_ns.epoch) ;

    [%log.debug (my log) "hello"[@@time Some time]];
    [%log (my log) "hello"[@@time time][@@level Some my_level]];
    [%log.format (my log) "hello world"];
    [%log.format (my log) "hello world %s" i]
    ;; |}]
;;

let command =
  let open Async in
  let find_files_based_on =
    Refactor_bash.Rg.create ~matching:(Regex {|(info|error|debug)_s|}) ()
  in
  let instance_impl actually_transform ~repo:_ ~project ~session:_ filename =
    if String.is_suffix ~suffix:".ml" (File_path.to_string filename)
    then (
      let%map editor = Refactor_project.find project ~filename in
      Refactor_editor.transform_exn editor ~f:(parse ~actually_transform))
    else return ()
  in
  Refactor_parallel.single_command
    ~find_files_based_on
    ~instance_impl
    ~shared_param:
      (Roundtrippable_command_param.create_required
         "actually-transform"
         Command.Param.Arg_type.Export.bool
         ~to_string:Bool.to_string
         ~doc:"Actually transform the code, vs. only parse and print errors")
    ~summary:
      "Replaces all Log[.Global].(info|error|debug)_s calls with [%message] arguments \
       with the ppx_log equivalents"
    ()
;;
