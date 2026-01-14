open! Core
open! Async

let test_locations () =
  [%log.info [%here]];
  [%log.info ([%here] : Source_code_position.t)];
  [%log.info "message" [%here]];
  [%log.info "message" ([%here] : Source_code_position.t)];
  [%log.info "message" ~h:[%here]];
  let here = [%here] in
  [%log.info "message" [@@loc here]]
;;

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
;;

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
;;
