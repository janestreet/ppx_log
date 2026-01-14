open! Core
open! Async

let log_output = Log.Output.stdout ~format:`Sexp_hum ()
let create_log ~level = Log.create ~level ~output:[ log_output ] ~on_error:`Raise ()

let%expect_test "logging with extra attributes" =
  let time = Time_float.of_span_since_epoch (sec 1.) in
  let my_level = `Error in
  let tags = [ "a", "b" ] in
  [%log "test" (5 : int) [@@tags tags] [@@time Some time] [@@level Some my_level]];
  let%bind () = Log.Global.flushed () in
  [%expect {| 1969-12-31 19:00:01.000000-05:00 Error (test(5 5)) -- [a: b] |}];
  return ()
;;

let%expect_test "ppx_string use" =
  Log.Global.For_testing.use_test_output ();
  let str = "world" in
  [%log.string "hello %{str} %{3#Int}"];
  let%bind () = Log.Global.flushed () in
  [%expect {| hello world 3 |}];
  return ()
;;

let%expect_test "*_sexp and *_format extensions" =
  Log.Global.For_testing.use_test_output ();
  let e = error_s [%message "hello" (5 : int)] in
  [%log.info_sexp (e : _ Or_error.t)];
  let%bind () = Log.Global.flushed () in
  [%expect {| (Error(hello(5 5))) |}];
  [%log.t.debug_sexp (force Log.Global.log) (e : _ Or_error.t)];
  let%bind () = Log.Global.flushed () in
  [%expect {| |}];
  [%log.info_format "hello %d" 5];
  let%bind () = Log.Global.flushed () in
  [%expect {| hello 5 |}];
  [%log.t.error_format (force Log.Global.log) "world %d" 10];
  let%bind () = Log.Global.flushed () in
  [%expect {| world 10 |}];
  (* The below is an edge case from a catalog test. The inner experession should be
     treated as a function application, not a list of arguments *)
  [%log.info_sexp Fn.id [%message "test"]];
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
    test (fun () -> [%log "test" [@@legacy_tag_parentheses]]) [%sexp "test"]
  in
  [%expect {| test |}];
  let i = 5 in
  let%bind () =
    test
      (fun () -> [%log "test" (i : int) [@@legacy_tag_parentheses]])
      [%sexp "test", { i : int }]
  in
  [%expect {| (test ((i 5))) |}];
  let%bind () =
    test
      (fun () -> [%log "test" (i : int) (i : int) [@@legacy_tag_parentheses]])
      [%sexp "test", { i : int; i : int }]
  in
  [%expect {| (test ((i 5) (i 5))) |}];
  let%bind () =
    test
      (fun () -> [%log "" (i : int) (i : int) [@@legacy_tag_parentheses]])
      [%sexp { i : int; i : int }]
  in
  [%expect {| ((i 5) (i 5)) |}];
  let%bind () =
    test (fun () -> [%log "" (i : int) [@@legacy_tag_parentheses]]) [%sexp { i : int }]
  in
  [%expect {| ((i 5)) |}];
  let%bind () =
    test (fun () -> [%log (i : int) [@@legacy_tag_parentheses]]) [%sexp { i : int }]
  in
  [%expect {| ((i 5)) |}];
  let%bind () =
    test
      (fun () -> [%log (i : int) (i : int) [@@legacy_tag_parentheses]])
      [%sexp { i : int; i : int }]
  in
  [%expect {| ((i 5) (i 5)) |}];
  let%bind () = test (fun () -> [%log "" [@@legacy_tag_parentheses]]) [%sexp ()] in
  [%expect {| () |}];
  let%bind () =
    test
      (fun () -> [%log.t (force Log.Global.log) ~a:123 [@@legacy_tag_parentheses]])
      [%sexp { a = 123 }]
  in
  [%expect {| ((a 123)) |}];
  let%bind () = test (fun () -> [%log 123 [@@legacy_tag_parentheses]]) [%sexp 123] in
  [%expect {| 123 |}];
  Deferred.unit
;;

let%expect_test "logging non-string literals (expected extremely rare / unused, but \
                 technically possible in [%message], so just here for demonstration)"
  =
  [%log 'c'];
  [%log '\000'];
  [%log 5];
  [%log 3.14e-1];
  let%bind () = Log.Global.flushed () in
  [%expect
    {|
    c
    "\000"
    5
    0.314
    |}];
  return ()
;;

let%expect_test "printf format string edge case" =
  (* A previous iteration of the ppx translated format extensions to [sprintf]s which have
     a slightly different format string type than [printf], and thus cause this kind of
     expression to not compile. *)
  let print s = if true then printf s else [%log.info_format s] in
  print "hello";
  [%expect {| hello |}];
  Deferred.unit
;;

let%expect_test "logging with raw_message and `Raw format" =
  let i = 1 in
  let raw_message = [%log.make_raw "hi" (i : int)] in
  let source, data = raw_message in
  [%log.t.raw (force Log.Global.log) raw_message];
  [%log.error_raw source, data];
  [%log.raw [%log.make_raw "hi" (i : int)]];
  let%bind () = Log.Global.flushed () in
  [%expect
    {|
    (hi(i 1))
    (hi(i 1))
    (hi(i 1))
    |}];
  (* Some users may want to render the message into a sexp for further use later. *)
  print_s (Ppx_log_types.Raw_message.sexp_of_t_hum raw_message);
  [%expect
    {|
    ((hi (i 1))
     ((source "ppx/ppx_log/test/test_log_extensions.ml:137 (Ppx_log_test)")))
    |}];
  return ()
;;

let%expect_test "sexp option" =
  let something = Some 5 in
  [%log (something : (int option[@sexp.option])) (None : (int option[@sexp.option]))];
  [%expect {| (something 5) |}];
  return ()
;;

let%expect_test "omit nil" =
  let nil = Sexp.List [] in
  let not_nil = Sexp.Atom "x" in
  [%log (nil : Sexp.t) (not_nil : Sexp.t) ~omitted_nil:(nil : (Sexp.t[@sexp.omit_nil]))];
  [%expect {| ((nil())(not_nil x)) |}];
  return ()
;;

let%expect_test "empty optional tags are dropped" =
  [%log
    ""
      ~some:(Some 1 : int option)
      ?some_opt:(Some 1 : int option)
      ~none:(None : int option)
      ?none_opt:(None : int option)];
  [%expect {| ((some(1))(some_opt 1)(none())) |}];
  return ()
;;

let%expect_test "optional tags are the same as sexp options" =
  [%log
    ""
      ~some:(Some 1 : (int option[@sexp.option]))
      ?some_opt:(Some 1 : int option)
      ~none:(None : (int option[@sexp.option]))
      ?none_opt:(None : int option)];
  [%expect {| ((some 1)(some_opt 1)) |}];
  return ()
;;

let%expect_test "logging nothing" =
  [%log "" ~_:(None : int option)];
  [%expect {| () |}];
  [%log "" ?nothing_at_all:(None : int option)];
  [%expect {| () |}];
  return ()
;;

module%test [@name "json"] _ = struct
  open Jsonaf_kernel.Conv

  type t = { users : string list } [@@deriving jsonaf_of]

  let%expect_test "printf format string edge case" =
    let my_t = { users = [ "me"; "you" ] } in
    [%log (my_t : (t[@j]))];
    [%expect {| (my_t(Object((users(Array((String me)(String you))))))) |}];
    Deferred.unit
  ;;

  let%expect_test "with sexp option" =
    let my_t = Some { users = [ "me"; "you" ] } in
    [%log (my_t : (t option[@j] [@sexp.option]))];
    [%expect {| (my_t(Object((users(Array((String me)(String you))))))) |}];
    [%log (None : (t option[@j] [@sexp.option]))];
    [%expect {| () |}];
    Deferred.unit
  ;;

  let%expect_test "optional tags" =
    let my_t : t option = Some { users = [ "us"; "them" ] } in
    [%log (my_t : (t option[@j]))];
    [%expect {| (my_t(Object((users(Array((String us)(String them))))))) |}];
    [%log "" ?not_my_t:(None : (t option[@j]))];
    [%expect {| () |}];
    [%log
      ""
        (my_t : (t option[@j]))
        ~null_t:(None : (t option[@j]))
        ?not_my_t:(None : (t option[@j]))];
    [%expect
      {| ((my_t(Object((users(Array((String us)(String them)))))))(null_t Null)) |}];
    Deferred.unit
  ;;
end
