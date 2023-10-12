open! Core
open! Async

let log_output = Log.Output.stdout ~format:`Sexp_hum ()
let create_log ~level = Log.create ~level ~output:[ log_output ] ~on_error:`Raise ()

let%expect_test "logging with extra attributes" =
  let time = Time_float.of_span_since_epoch (sec 1.) in
  let my_level = `Error in
  let tags = [ "a", "b" ] in
  [%log.global "test" (5 : int) [@@tags tags] [@@time Some time] [@@level Some my_level]];
  let%bind () = Log.Global.flushed () in
  [%expect {| 1969-12-31 19:00:01.000000-05:00 Error (test(5 5)) -- [a: b] |}];
  return ()
;;

let%expect_test "*_sexp and *_format extensions" =
  Log.Global.For_testing.use_test_output ();
  let e = error_s [%message "hello" (5 : int)] in
  [%log.global.info_sexp (e : _ Or_error.t)];
  let%bind () = Log.Global.flushed () in
  [%expect {| (Error(hello(5 5))) |}];
  [%log.debug_sexp (force Log.Global.log) (e : _ Or_error.t)];
  let%bind () = Log.Global.flushed () in
  [%expect {| |}];
  [%log.global.info_format "hello %d" 5];
  let%bind () = Log.Global.flushed () in
  [%expect {| hello 5 |}];
  [%log.error_format (force Log.Global.log) "world %d" 10];
  let%bind () = Log.Global.flushed () in
  [%expect {| world 10 |}];
  (* The below is an edge case from a catalog test. The inner experession should be
     treated as a function application, not a list of arguments *)
  [%log.global.info_sexp Fn.id [%message "test"]];
  let%bind () = Log.Global.flushed () in
  [%expect {| test |}];
  return ()
;;

let%expect_test "legacy tag parentheses" =
  Log.Global.For_testing.use_test_output ();
  let test log expect =
    log ();
    let%map () = Log.Global.flushed () in
    [%test_result: Sexp.t] ([%expect.output] |> Sexp.of_string) ~expect;
    print_s expect
  in
  let%bind () =
    test (fun () -> [%log.global "test" [@@legacy_tag_parentheses]]) [%sexp "test"]
  in
  [%expect {| test |}];
  let i = 5 in
  let%bind () =
    test
      (fun () -> [%log.global "test" (i : int) [@@legacy_tag_parentheses]])
      [%sexp "test", { i : int }]
  in
  [%expect {| (test ((i 5))) |}];
  let%bind () =
    test
      (fun () -> [%log.global "test" (i : int) (i : int) [@@legacy_tag_parentheses]])
      [%sexp "test", { i : int; i : int }]
  in
  [%expect {| (test ((i 5) (i 5))) |}];
  let%bind () =
    test
      (fun () -> [%log.global "" (i : int) (i : int) [@@legacy_tag_parentheses]])
      [%sexp { i : int; i : int }]
  in
  [%expect {| ((i 5) (i 5)) |}];
  let%bind () =
    test
      (fun () -> [%log.global "" (i : int) [@@legacy_tag_parentheses]])
      [%sexp { i : int }]
  in
  [%expect {| ((i 5)) |}];
  let%bind () =
    test
      (fun () -> [%log.global (i : int) [@@legacy_tag_parentheses]])
      [%sexp { i : int }]
  in
  [%expect {| ((i 5)) |}];
  let%bind () =
    test
      (fun () -> [%log.global (i : int) (i : int) [@@legacy_tag_parentheses]])
      [%sexp { i : int; i : int }]
  in
  [%expect {| ((i 5) (i 5)) |}];
  let%bind () = test (fun () -> [%log.global "" [@@legacy_tag_parentheses]]) [%sexp ()] in
  [%expect {| () |}];
  Deferred.unit
;;
