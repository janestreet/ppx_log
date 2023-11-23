open! Base
open! Import

module Label : sig
  type t =
    | String of string
    | String_literal of string
end

(** A [t] represents a [%message] sexp with additional structured information that
    distinguishes the message label from the typed list of tags that follow. *)
type t

val create
  :  ?legacy_render_with_additional_parentheses:bool
  -> Label.t option
  -> tags:Log_tag.t list
  -> t

(** [label] is [None] in cases like [%log log (tag1 : t1) ...] or [%log.global "" (tag1 :
    t1) ...]. *)
val label : t -> Label.t option

val tags : t -> Log_tag.t list
val render : t -> Sexp.t

module Unstable : sig
  (** [sexp_of] is here since we explicitly do not want this serialized in a way where
      users may expect to be able to deserialize. *)
  type nonrec t = t [@@deriving sexp_of]
end
