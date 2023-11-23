(* This library exists separately from [Async_log], and even [Core], because [ppx_log] is
   part of [ppx_jane], and numerous dependencies of [Core] use [ppx_jane]. *)

open! Base
open! Import
open Sexplib
module Level = Level
module Log_tag = Log_tag
module Message_sexp = Message_sexp
module Message_source = Message_source
module Tag_data = Tag_data

module type S = sig
  type t
  type time
  type return_type

  (* [would_log] and [Global.would_log] take an option because [Async.Log.would_log] takes
     an option and we want to pass in a [Some `Debug] statically so that it won't
     allocate. If we made a wrapper function that always just wrapped the level in [Some],
     it might allocate. *)
  val would_log : t -> Level.t option -> bool
  val default : return_type

  val printf
    :  ?level:Level.t
    -> ?time:time
    -> ?tags:(string * string) list
    -> t
    -> ('a, unit, string, return_type) format4
    -> 'a

  val sexp
    :  ?level:Level.t
    -> ?time:time
    -> ?tags:(string * string) list
    -> t
    -> Sexp.t
    -> return_type

  val message
    :  ?level:Level.t
    -> ?time:time
    -> ?tags:(string * string) list
    -> t
    -> Message_sexp.t
    -> Message_source.t
    -> return_type

  module Global : sig
    type return_type

    val would_log : Level.t option -> bool
    val default : return_type

    val printf
      :  ?level:Level.t
      -> ?time:time
      -> ?tags:(string * string) list
      -> ('a, unit, string, return_type) format4
      -> 'a

    val message
      :  ?level:Level.t
      -> ?time:time
      -> ?tags:(string * string) list
      -> Message_sexp.t
      -> Message_source.t
      -> return_type

    val sexp
      :  ?level:Level.t
      -> ?time:time
      -> ?tags:(string * string) list
      -> Sexp.t
      -> return_type
  end
end
