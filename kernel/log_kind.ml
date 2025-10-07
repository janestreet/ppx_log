open! Base
open! Import
open Ppxlib

type 'data t =
  [ `Instance of 'data
  | `Global
  | `Explicit_global
  ]
[@@deriving enumerate]

let extension_prefix (type data) = function
  | `Instance (_ : data) -> "@log.t"
  | `Global -> "@log"
  | `Explicit_global -> "@log.global"
;;

let would_log log ~level ~loc =
  let level = Optional_arg.to_expr level ~loc in
  match log with
  | `Instance log -> [%expr Ppx_log_syntax.Instance.would_log [%e log] [%e level]]
  | `Global | `Explicit_global -> [%expr Ppx_log_syntax.Global.would_log [%e level]]
;;

let log_function (type data) t ~loc =
  match t with
  | `Instance (_ : data) -> [%expr Ppx_log_syntax.Instance.message]
  | `Global | `Explicit_global -> [%expr Ppx_log_syntax.Global.message]
;;

let log_default (type data) t ~loc =
  match t with
  | `Instance (_ : data) -> [%expr Ppx_log_syntax.Instance.default]
  | `Global | `Explicit_global -> [%expr Ppx_log_syntax.Global.default]
;;

let log_arg = function
  | `Global | `Explicit_global -> None
  | `Instance log -> Some (Nolabel, log)
;;
