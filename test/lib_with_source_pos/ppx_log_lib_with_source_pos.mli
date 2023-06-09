open! Core
open! Async

val log_global_info : ?tags:(string * string) list -> string -> unit
val log_info : Log.t -> ?tags:(string * string) list -> string -> unit
