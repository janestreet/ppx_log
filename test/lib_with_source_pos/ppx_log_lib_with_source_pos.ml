open! Core
open! Async

let log_global_info ?tags msg =
  match tags with
  | None -> [%log.global.info msg]
  | Some tags -> [%log.global.info msg [@@tags tags]]
;;

let log_info log ?tags msg =
  match tags with
  | None -> [%log.t.info log msg]
  | Some tags -> [%log.t.info log msg [@@tags tags]]
;;
