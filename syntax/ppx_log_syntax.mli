(** [ppx_log] translates to code that references a [module Ppx_log_syntax :
    Ppx_log_types.S] expected to be in scope. Normally, this is provided by an actual log
    implementation, like [Async_log]. If the module isn't in scope, this library provides
    a stub implementation with an alert to direct users to the right place. *)

open! Base

type t
type time
type return_type = [ `Ppx_log_syntax_implementation_missing ]

val would_log : t -> Ppx_log_types.Level.t option -> bool
val default : return_type

val message
  :  ?level:Ppx_log_types.Level.t
  -> ?time:time
  -> ?tags:(string * string) list
  -> t
  -> Ppx_log_types.Message_data.t
  -> Ppx_log_types.Message_source.t
  -> return_type
  [@@alert
    ppx_log_syntax_not_in_scope
      "In order to use [ppx_log], you need to have a [Ppx_log_syntax] in scope. This is \
       usually provided with [open Async], [open Async_log.Ppx_log_syntax], or [open \
       Async_log_kernel.Ppx_log_syntax]."]

module Global : sig
  val would_log : Ppx_log_types.Level.t option -> bool
  val default : return_type

  val message
    :  ?level:Ppx_log_types.Level.t
    -> ?time:time
    -> ?tags:(string * string) list
    -> Ppx_log_types.Message_data.t
    -> Ppx_log_types.Message_source.t
    -> return_type
    [@@alert
      ppx_log_syntax_not_in_scope
        "In order to use [ppx_log], you need to have a [Ppx_log_syntax] in scope. This \
         is usually provided with [open Async], [open Async_log.Ppx_log_syntax], or \
         [open Async_log_kernel.Ppx_log_syntax]."]
end
