open! Core
open! Async

let log_output = Log.Output.stdout ~format:`Sexp_hum ()
let create_log ~level = Log.create ~level ~output:[ log_output ] ~on_error:`Raise ()

let%expect_test "logs source position from opted-in library" =
  let log = create_log ~level:`Info in
  Ppx_log_lib_with_source_pos.log_info log "expect position in tags";
  let%bind () = Log.flushed log in
  [%expect
    {|
    (V2
     ((time (1969-12-31 19:00:00.000000-05:00)) (level (Info))
      (message (Sexp "expect position in tags"))
      (tags
       ((pos
         ppx/ppx_log/test/lib_with_source_pos/ppx_log_lib_with_source_pos.ml:12:12)))))
    |}];
  return ()
;;

let%expect_test "logs no source position from current library" =
  let log = create_log ~level:`Info in
  [%log.info log "expect NO position in tags"];
  let%bind () = Log.flushed log in
  [%expect
    {|
    (V2
     ((time (1969-12-31 19:00:00.000000-05:00)) (level (Info))
      (message (Sexp "expect NO position in tags")) (tags ())))
    |}];
  return ()
;;

let%expect_test "globally logs source position from opted-in library" =
  Log.Global.set_output [ log_output ];
  Ppx_log_lib_with_source_pos.log_global_info "expect position in tags";
  let%bind () = Log.Global.flushed () in
  [%expect
    {|
    (V2
     ((time (1969-12-31 19:00:00.000000-05:00)) (level (Info))
      (message (Sexp "expect position in tags"))
      (tags
       ((pos
         ppx/ppx_log/test/lib_with_source_pos/ppx_log_lib_with_source_pos.ml:6:12)))))
    |}];
  return ()
;;

let%expect_test "globally logs source position and other tags from opted-in library" =
  Log.Global.set_output [ log_output ];
  Ppx_log_lib_with_source_pos.log_global_info
    ~tags:[ "another_tag", "hi" ]
    "expect position in tags";
  let%bind () = Log.Global.flushed () in
  [%expect
    {|
    (V2
     ((time (1969-12-31 19:00:00.000000-05:00)) (level (Info))
      (message (Sexp "expect position in tags"))
      (tags
       ((pos
         ppx/ppx_log/test/lib_with_source_pos/ppx_log_lib_with_source_pos.ml:7:17)
        (another_tag hi)))))
    |}];
  let log = Lazy.force Log.Global.log in
  Ppx_log_lib_with_source_pos.log_info
    log
    ~tags:[ "another_tag", "hi" ]
    "expect position in tags";
  let%bind () = Log.flushed log in
  [%expect
    {|
    (V2
     ((time (1969-12-31 19:00:00.000000-05:00)) (level (Info))
      (message (Sexp "expect position in tags"))
      (tags
       ((pos
         ppx/ppx_log/test/lib_with_source_pos/ppx_log_lib_with_source_pos.ml:13:17)
        (another_tag hi)))))
    |}];
  return ()
;;

let%expect_test "globally logs no source position from current library" =
  Log.Global.set_output [ log_output ];
  [%log.global.info "expect NO position in tags"];
  let%bind () = Log.Global.flushed () in
  [%expect
    {|
    (V2
     ((time (1969-12-31 19:00:00.000000-05:00)) (level (Info))
      (message (Sexp "expect NO position in tags")) (tags ())))
    |}];
  return ()
;;

let%expect_test "globally logs no source position and other tags from current library" =
  Log.Global.set_output [ log_output ];
  [%log.global.info "expect NO position in tags" [@@tags [ "another tag", "hi" ]]];
  let%bind () = Log.Global.flushed () in
  [%expect
    {|
    (V2
     ((time (1969-12-31 19:00:00.000000-05:00)) (level (Info))
      (message (Sexp "expect NO position in tags")) (tags (("another tag" hi)))))
    |}];
  let log = Lazy.force Log.Global.log in
  [%log.info log "expect NO position in tags" [@@tags [ "another tag", "hi" ]]];
  let%bind () = Log.flushed log in
  [%expect
    {|
    (V2
     ((time (1969-12-31 19:00:00.000000-05:00)) (level (Info))
      (message (Sexp "expect NO position in tags")) (tags (("another tag" hi)))))
    |}];
  return ()
;;
