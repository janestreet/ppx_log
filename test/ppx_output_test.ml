open! Core
open! Async

[@@@warning "-unused-var-strict"]

[@@@expand_inline
  let test_locations () =
    [%log.info [%here]];
    [%log.info ([%here] : Source_code_position.t)];
    [%log.info "message" [%here]];
    [%log.info "message" ([%here] : Source_code_position.t)];
    [%log.info "message" ~h:[%here]];
    let here = [%here] in
    [%log.info "message" [@@loc here]]
  ;;]

let test_locations () =
  if Ppx_log_syntax.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create
              None
              ~tags:
                [ { Ppx_log_types.Log_tag.name = ""
                  ; data = String "ppx/ppx_log/test/ppx_output_test.ml:8:15"
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:8
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create
              None
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "[%here ]"
                  ; data =
                      Ppx_log_types.Tag_data.Sexp
                        ((Source_code_position.sexp_of_t [@merlin.hide])
                           { Ppx_here_lib.pos_fname =
                               "ppx/ppx_log/test/ppx_output_test.ml"
                           ; pos_lnum = 9
                           ; pos_cnum = 144
                           ; pos_bol = 128
                           })
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:9
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String_literal "message"))
              ~tags:
                [ { Ppx_log_types.Log_tag.name = ""
                  ; data = String "ppx/ppx_log/test/ppx_output_test.ml:10:25"
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:10
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String_literal "message"))
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "[%here ]"
                  ; data =
                      Ppx_log_types.Tag_data.Sexp
                        ((Source_code_position.sexp_of_t [@merlin.hide])
                           { Ppx_here_lib.pos_fname =
                               "ppx/ppx_log/test/ppx_output_test.ml"
                           ; pos_lnum = 11
                           ; pos_cnum = 241
                           ; pos_bol = 215
                           })
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:11
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String_literal "message"))
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "h"
                  ; data = String "ppx/ppx_log/test/ppx_output_test.ml:12:28"
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:12
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  let here =
    { Ppx_here_lib.pos_fname = "ppx/ppx_log/test/ppx_output_test.ml"
    ; pos_lnum = 13
    ; pos_cnum = 330
    ; pos_bol = 315
    }
  in
  if Ppx_log_syntax.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create (Some (String_literal "message")) ~tags:[]))
         (Ppx_log_types.Message_source.Private.of_source_code_position
            here [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default
;;

[@@@end]

[@@@expand_inline
  let test () =
    [%log.debug "test" (5 : int)];
    let msg = "test" in
    [%log.debug msg (5 : int)];
    [%log.info (5 : int)];
    [%log.error "test" [@@tags []]];
    let log = force Log.Global.log in
    [%log.t.debug log "test" (5 : int) [@@tags [ "hello", "world" ]]];
    [%log.t.info log (5 : int)];
    let my_tags = [ "a", "b" ] in
    [%log.t.error log "test" [@@tags my_tags]];
    [%log.info "test" ?some:(Some 5 : int option) ?none:(None : int option)]
  ;;]

let test () =
  if Ppx_log_syntax.Global.would_log (Some `Debug) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Debug
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String_literal "test"))
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "5"
                  ; data = Ppx_log_types.Tag_data.Int 5
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:171
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  let msg = "test" in
  if Ppx_log_syntax.Global.would_log (Some `Debug) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Debug
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String msg))
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "5"
                  ; data = Ppx_log_types.Tag_data.Int 5
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:173
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create
              None
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "5"
                  ; data = Ppx_log_types.Tag_data.Int 5
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:174
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Global.would_log (Some `Error) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Error
         ~tags:[]
         (`Structured
           (Ppx_log_types.Message_sexp.create (Some (String_literal "test")) ~tags:[]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:175
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  let log = force Log.Global.log in
  if Ppx_log_syntax.Instance.would_log log (Some `Debug) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Instance.message
         ~level:`Debug
         ~tags:[ "hello", "world" ]
         log
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String_literal "test"))
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "5"
                  ; data = Ppx_log_types.Tag_data.Int 5
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:177
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Instance.default;
  if Ppx_log_syntax.Instance.would_log log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Instance.message
         ~level:`Info
         log
         (`Structured
           (Ppx_log_types.Message_sexp.create
              None
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "5"
                  ; data = Ppx_log_types.Tag_data.Int 5
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:178
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Instance.default;
  let my_tags = [ "a", "b" ] in
  if Ppx_log_syntax.Instance.would_log log (Some `Error) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Instance.message
         ~level:`Error
         ~tags:my_tags
         log
         (`Structured
           (Ppx_log_types.Message_sexp.create (Some (String_literal "test")) ~tags:[]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:180
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Instance.default;
  if Ppx_log_syntax.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String_literal "test"))
              ~tags:
                (match
                   ( (match Some 5 with
                      | None -> None
                      | Some value -> Some (Ppx_log_types.Tag_data.Int value))
                   , match
                       ( (match None with
                          | None -> None
                          | Some value -> Some (Ppx_log_types.Tag_data.Int value))
                       , [] )
                     with
                     | None, tl -> tl
                     | Some data, tl ->
                       { Ppx_log_types.Log_tag.name = "none"; data } :: tl )
                 with
                 | None, tl -> tl
                 | Some data, tl -> { Ppx_log_types.Log_tag.name = "some"; data } :: tl)))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:181
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default
;;

[@@@end]

[@@@expand_inline
  let test_extensions () =
    [%log.format "test %d" 3];
    [%log.error_format "world %s" "yes"];
    let log = force Log.Global.log in
    let generate_sexp () = Sexp.Atom "" in
    [%log.t.sexp log (5 : int)];
    [%log.t.sexp log (generate_sexp ())];
    let my_level = Some `Debug in
    [%log.t log "test" [@@time Some Time_float.epoch] [@@level my_level]];
    [%log.string 3 |> Int.to_string];
    [%log.string 3 |> Int.to_string];
    [%log.t.string log (Int.to_string 3)];
    [%log.sexp 3 |> Int.sexp_of_t];
    let raw_message = [%log.make_raw "hi" (1 : int) (2 : int) (3 : int)] in
    [%log.error_raw raw_message]
  ;;]

let test_extensions () =
  if Ppx_log_syntax.Global.would_log None [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Printf.ksprintf
         (fun str ->
           Ppx_log_syntax.Global.message
             (`String str)
             (Ppx_log_types.Message_source.Private.code
                ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
                ~pos_lnum:376
                ~module_name:Stdlib.__MODULE__ [@merlin.hide]))
         "test %d"
         3
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Global.would_log (Some `Error) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Printf.ksprintf
         (fun str ->
           Ppx_log_syntax.Global.message
             ~level:`Error
             (`String str)
             (Ppx_log_types.Message_source.Private.code
                ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
                ~pos_lnum:377
                ~module_name:Stdlib.__MODULE__ [@merlin.hide]))
         "world %s"
         "yes"
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  let log = force Log.Global.log in
  let generate_sexp () = Sexp.Atom "" in
  if Ppx_log_syntax.Instance.would_log log None [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Instance.message
         log
         (`Sexp ((sexp_of_int [@merlin.hide]) 5))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:380
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Instance.default;
  if Ppx_log_syntax.Instance.would_log log None [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Instance.message
         log
         (`Sexp (generate_sexp ()))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:381
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Instance.default;
  let my_level = Some `Debug in
  if Ppx_log_syntax.Instance.would_log log my_level [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Instance.message
         ?level:my_level
         ?time:(Some Time_float.epoch)
         log
         (`Structured
           (Ppx_log_types.Message_sexp.create (Some (String_literal "test")) ~tags:[]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:383
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Instance.default;
  if Ppx_log_syntax.Global.would_log None [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         (`String (Int.to_string 3))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:384
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Global.would_log None [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         (`String (Int.to_string 3))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:385
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  if Ppx_log_syntax.Instance.would_log log None [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Instance.message
         log
         (`String (Int.to_string 3))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:386
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Instance.default;
  if Ppx_log_syntax.Global.would_log None [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         (`Sexp (Int.sexp_of_t 3))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:387
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default;
  let raw_message =
    ( (Ppx_log_types.Message_source.Private.code
         ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
         ~pos_lnum:388
         ~module_name:Stdlib.__MODULE__ [@merlin.hide])
    , `Structured
        (Ppx_log_types.Message_sexp.create
           (Some (String_literal "hi"))
           ~tags:
             [ { Ppx_log_types.Log_tag.name = "1"; data = Ppx_log_types.Tag_data.Int 1 }
             ; { Ppx_log_types.Log_tag.name = "2"; data = Ppx_log_types.Tag_data.Int 2 }
             ; { Ppx_log_types.Log_tag.name = "3"; data = Ppx_log_types.Tag_data.Int 3 }
             ]) )
  in
  if Ppx_log_syntax.Global.would_log (Some `Error) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Global.message
         ~level:`Error
         (match raw_message with
          | message_source, message_data -> message_data)
         (match raw_message with
          | message_source, message_data -> message_source)
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Global.default
;;

[@@@end]

[@@@expand_inline
  let test_portable_extensions () =
    [%log.portable.info "portable" (5 : int)];
    let log = force Log.Global.log in
    [%log.portable.t.warn log "portable instance" [@@tags [ "hello", "world" ]]];
    [%log.portable.error_format "portable format %d" 3]
  ;;]

let test_portable_extensions () =
  if Ppx_log_syntax.Portable.Global.would_log (Some `Info) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Portable.Global.message
         ~level:`Info
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String_literal "portable"))
              ~tags:
                [ { Ppx_log_types.Log_tag.name = "5"
                  ; data = Ppx_log_types.Tag_data.Int 5
                  }
                ]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:572
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Portable.Global.default;
  let log = force Log.Global.log in
  if Ppx_log_syntax.Portable.Instance.would_log log (Some `Warn) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Ppx_log_syntax.Portable.Instance.message
         ~level:`Warn
         ~tags:[ "hello", "world" ]
         log
         (`Structured
           (Ppx_log_types.Message_sexp.create
              (Some (String_literal "portable instance"))
              ~tags:[]))
         (Ppx_log_types.Message_source.Private.code
            ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
            ~pos_lnum:574
            ~module_name:Stdlib.__MODULE__ [@merlin.hide])
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Portable.Instance.default;
  if Ppx_log_syntax.Portable.Global.would_log (Some `Error) [@merlin.hide]
  then (
    (let ppx_log_statement () =
       Printf.ksprintf
         (fun str ->
           Ppx_log_syntax.Portable.Global.message
             ~level:`Error
             (`String str)
             (Ppx_log_types.Message_source.Private.code
                ~pos_fname:"ppx/ppx_log/test/ppx_output_test.ml"
                ~pos_lnum:575
                ~module_name:Stdlib.__MODULE__ [@merlin.hide]))
         "portable format %d"
         3
         [@@cold]
     in
     ppx_log_statement () [@nontail])
    [@merlin.hide])
  else Ppx_log_syntax.Portable.Global.default
;;

[@@@end]
