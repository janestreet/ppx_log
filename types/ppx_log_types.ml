(* This library exists separately from [Async_log], and even [Core], because [ppx_log] is
   part of [ppx_jane], and numerous dependencies of [Core] use [ppx_jane]. *)

open! Base
open! Import
module Level = Level
module Log_tag = Log_tag
module Message_data = Message_data
module Message_sexp = Message_sexp
module Message_source = Message_source
module Raw_message = Raw_message
module Tag_data = Tag_data

module type S = sig
  type time

  (** A use of the ppx like [[%log.t t ...]] is converted to code like:
      {[
        if Ppx_log_syntax.Instance.would_log t level
        then Ppx_log_syntx.Instance.message t args
        else Ppx_log_syntax.Instance.default
      ]} *)
  module Instance : sig
    type t
    type return_type

    (* [would_log] and [Global.would_log] take an option because [Async.Log.would_log]
       takes an option and we want to pass in a [Some `Debug] statically so that it won't
       allocate. If we made a wrapper function that always just wrapped the level in
       [Some], it might allocate. *)
    val would_log : t -> Level.t option -> bool
    val default : return_type

    val message
      :  ?level:Level.t
      -> ?time:time
      -> ?tags:(string * string) list
      -> t
      -> Message_data.t
      -> Message_source.t
      -> return_type
  end

  (** A use of the ppx like [[%log.global ...]] is converted to code like:
      {[
        if Ppx_log_syntax.Global.would_log level
        then Ppx_log_syntx.Global.message args
        else Ppx_log_syntax.Global.default
      ]} *)
  module Global : sig
    type return_type

    val would_log : Level.t option -> bool
    val default : return_type

    val message
      :  ?level:Level.t
      -> ?time:time
      -> ?tags:(string * string) list
      -> Message_data.t
      -> Message_source.t
      -> return_type
  end
end

open Sexplib

let (_ : Sexp.t) = List []
