open Base
open Sexplib

type level =
  [ `Debug
  | `Info
  | `Error
  ]

module type S = sig
  type t
  type time
  type return_type

  (* [would_log] and [Global.would_log] take an option because of
     [Async.Log.would_log] takes an option and we want to pass in a (Some
     `Debug) statically so that it won't allocate. If we made a wrapper
     function that always just wrapped the level in `Some, it might allocate.
  *)
  val would_log : t -> level option -> bool
  val default : return_type

  val printf
    :  ?level:level
    -> ?time:time
    -> ?tags:(string * string) list
    -> t
    -> ('a, unit, string, return_type) format4
    -> 'a

  val sexp
    :  ?level:level
    -> ?time:time
    -> ?tags:(string * string) list
    -> t
    -> Sexp.t
    -> return_type

  module Global : sig
    type return_type

    val would_log : level option -> bool
    val default : return_type

    val printf
      :  ?level:level
      -> ?time:time
      -> ?tags:(string * string) list
      -> ('a, unit, string, return_type) format4
      -> 'a

    val sexp
      :  ?level:level
      -> ?time:time
      -> ?tags:(string * string) list
      -> Sexp.t
      -> return_type
  end
end
