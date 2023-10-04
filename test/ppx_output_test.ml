open! Core
open! Async

let test () =
  [%log.global.debug "test" (5 : int)];
  [%log.global.info (5 : int)];
  [%log.global.error "test" [@@tags []]];
  let log = force Log.Global.log in
  [%log.debug log "test" (5 : int) [@@tags [ "hello", "world" ]]];
  [%log.info log (5 : int)];
  let my_tags = [ "a", "b" ] in
  [%log.error log "test" [@@tags my_tags]]
;;

let test_extensions () =
  [%log.global.format "test %d" 3];
  [%log.global.error_format "world %s" "yes"];
  let log = force Log.Global.log in
  let generate_sexp () = Sexp.Atom "" in
  [%log.sexp log (5 : int)];
  [%log.sexp log (generate_sexp ())];
  let my_level = Some `Debug in
  [%log log "test" [@@time Some Time_float.epoch] [@@level my_level]]
;;
