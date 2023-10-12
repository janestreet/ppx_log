open Core
open Async

let i = Some 5
let error_str = "error"
let host = "my host"
let e = Error.create_s [%message "my error"]
let errors = [ e ]
let error_sexp = [%sexp (e : Error.t)]
let log () = force Log.Global.log
let my_message = "my message"
let j = 5
let time = Time_float.epoch
let opt_time = None
let my_tags = [ "hello", "world" ]
let my_level = `Debug
let my_sexp () = [%sexp "hello"]

let do_log () =
  Log.Global.info_s [%message error_str];
  Log.Global.info_s [%message (host : String.t) (e : Error.t)] ~tags:[ "hello2", "world" ];
  Log.Global.info_s [%message "message3" ~_:(List.hd_exn errors : Error.t)];
  Log.Global.info_s [%message "message4" (i : (int option[@sexp.option]))];
  Log.Global.info_s [%message "message5" (i : (int option[@sexp.omit_nil]))];
  Log.Global.info_s [%sexp (e : Error.t)];
  Log.Global.info_s error_sexp;
  Log.Global.info_s [%sexp "message8", (host : String.t), ~~(e : Error.t)];
  Log.Global.info_s [%sexp (my_message : string), (host : String.t)];
  Log.Global.info_s [%sexp (my_message : string), { host : String.t }];
  Log.Global.info_s [%sexp [%string "hello11 %{j#Int}"], { host : String.t }];
  Log.Global.sexp ~level:`Debug ~time ~tags:my_tags [%message "hello12"];
  Log.Global.sexp ~level:my_level ?time:opt_time ~tags:my_tags [%message "hello13"];
  Log.Global.printf "hello world14";
  Log.Global.printf "hello world15 %d" j;
  Log.info_s (log ()) [%message error_str];
  Log.info_s
    (log ())
    [%message (host : String.t) (e : Error.t)]
    ~tags:[ "hello17", "world" ];
  Log.info_s (log ()) [%message "message18" ~_:(List.hd_exn errors : Error.t)];
  Log.info_s (log ()) [%message "message19" (i : (int option[@sexp.option]))];
  Log.info_s (log ()) [%message "message20" (i : (int option[@sexp.omit_nil]))];
  Log.info_s (log ()) [%sexp (e : Error.t)];
  Log.info_s (log ()) (my_sexp ());
  Log.info_s (log ()) error_sexp;
  Log.info_s (log ()) [%sexp "message24", (host : String.t), ~~(e : Error.t)];
  Log.info_s (log ()) [%sexp (my_message : string), (host : String.t)];
  Log.info_s (log ()) [%sexp (my_message : string), { host : String.t }];
  Log.info_s (log ()) [%sexp { a = 123 }];
  Log.info_s (log ()) [%sexp { j = (i : int option) }];
  Log.sexp (log ()) ~level:`Debug ~time [%message "hello29"] ~tags:my_tags;
  Log.sexp (log ()) ~level:my_level ?time:opt_time [%message "hello30"] ~tags:my_tags;
  Log.printf (log ()) "hello world31";
  Log.printf (log ()) "hello world32 %d" j;
  Log.Global.flushed ()
;;
