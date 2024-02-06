open! Base
open! Import

type t =
  { name : string
  ; data : Tag_data.t
  }

val of_pair : string * Tag_data.t -> t
val string_pair : string * string -> t

module Verbose : sig
  type nonrec t = t [@@deriving sexp_of]
end

module For_message_sexp : sig
  type nonrec t = t [@@deriving sexp_of]
end
