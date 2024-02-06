open! Base
open! Import
open Ppxlib

type 'data t =
  [ `Instance of 'data
  | `Global
  ]
[@@deriving enumerate]

let extension_prefix (type data) = function
  | `Instance (_ : data) -> "@log"
  | `Global -> "@log.global"
;;

let would_log (type data) log ~level ~loc =
  let level = Optional_arg.to_expr level ~loc in
  match log with
  | `Instance log -> [%expr Ppx_log_syntax.would_log [%e log] [%e level]]
  | `Global -> [%expr Ppx_log_syntax.Global.would_log [%e level]]
;;

let log_function (type data) t ~loc =
  match t with
  | `Instance (_ : data) -> [%expr Ppx_log_syntax.message]
  | `Global -> [%expr Ppx_log_syntax.Global.message]
;;

let log_default (type data) t ~loc =
  match t with
  | `Instance (_ : data) -> [%expr Ppx_log_syntax.default]
  | `Global -> [%expr Ppx_log_syntax.Global.default]
;;

let log_arg = function
  | `Global -> None
  | `Instance log -> Some (Nolabel, log)
;;
