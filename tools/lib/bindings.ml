open! Core
open Ppxlib
include Refactor_syntax.Bindings

let loc = Location.none

let process_structure initial_bindings editor (structure : structure) =
  (* This is a bit hacky - we add something to the end of the structure, then inspect the
     bindings from within the class when we process that last structure. I can't think of
     an easy way to extract the bindings. *)
  let final_bindings = ref initial_bindings in
  structure @ [ [%stri ()] ]
  |> (object
        inherit
          [_] Refactor_syntax.iter_with_bindings
            editor
            ~initial_bindings
            ~interpret_as:Value as super

        method! structure_item item =
          super#structure_item item;
          final_bindings := bindings
     end)
       #structure;
  !final_bindings
;;

let process_ml_file_exn initial_bindings editor =
  match Refactor_editor.parse_exn editor with
  | ML structure -> process_structure initial_bindings editor structure
  | MLI _ | MLT _ -> failwith "Expected ML file to parse as ML"
;;

let%expect_test "process_ml_file_exn" =
  let test bindings contents =
    let editor =
      Refactor_editor.create
        ~filename:(File_path.of_string "file.ml")
        ~original_contents:contents
    in
    let bindings =
      List.map bindings ~f:(Tuple2.map_fst ~f:Longident.parse)
      |> Refactor_syntax.Bindings.of_alist_exn
      |> Fn.flip process_ml_file_exn editor
    in
    print_s [%sexp (bindings : string Refactor_syntax.Bindings.t)]
  in
  let bindings =
    [ "Async.Log", "-> async log"
    ; "Async.Log.Global", "-> global log"
    ; "Log", "is not defined"
    ]
  in
  test bindings {| |};
  [%expect
    {|
    ((Async.Log "-> async log") (Async.Log.Global "-> global log")
     (Log "is not defined"))
    |}];
  test bindings {| module Log = Concord_log |};
  [%expect {| ((Async.Log "-> async log") (Async.Log.Global "-> global log")) |}];
  test bindings {| module Log = struct include Async.Log end |};
  [%expect
    {|
    ((Async.Log "-> async log") (Async.Log.Global "-> global log")
     (Log "-> async log") (Log.Global "-> global log"))
    |}];
  test bindings {| module Log = Async.Log |};
  [%expect
    {|
    ((Async.Log "-> async log") (Async.Log.Global "-> global log")
     (Log "-> async log") (Log.Global "-> global log"))
    |}];
  test bindings {| open Async module Log = Log.Global |};
  [%expect
    {|
    ((Async.Log "-> async log") (Async.Log.Global "-> global log")
     (Log "-> global log"))
    |}];
  test bindings {| module Log = Async.Log.Global |};
  [%expect
    {|
    ((Async.Log "-> async log") (Async.Log.Global "-> global log")
     (Log "-> global log"))
    |}];
  (* It's not quite right that [open Async] causes [Import.Log] to point to async log, but
     [Bindings] currently doesn't make a distinction between what's exported vs. what's in
     scope. So this is good enough I think. *)
  test bindings {| open Async |};
  [%expect
    {|
    ((Async.Log "-> async log") (Async.Log.Global "-> global log")
     (Log "-> async log") (Log.Global "-> global log"))
    |}];
  test bindings {| module Log = Async.Log.Global |};
  [%expect
    {|
    ((Async.Log "-> async log") (Async.Log.Global "-> global log")
     (Log "-> global log"))
    |}]
;;
