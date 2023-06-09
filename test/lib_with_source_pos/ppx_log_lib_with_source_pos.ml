open! Core
open! Async

let log_global_info ?tags msg =
  match tags with
  | None -> [%log.global.info msg]
  | Some tags -> [%log.global.info msg [@@tags tags]]
;;

let log_info log ?tags msg =
  match tags with
  | None -> [%log.info log msg]
  | Some tags -> [%log.info log msg [@@tags tags]]
;;
