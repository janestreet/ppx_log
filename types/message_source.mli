open! Base
open! Import

module Code_position : sig
  type t = private
    { pos_fname : string
    ; pos_lnum : int
    ; library_name : string
    }
end

type t =
  | Manually_constructed of string
  | Code of Code_position.t
[@@deriving sexp_of]

module Private : sig
  val code : pos_fname:string -> pos_lnum:int -> module_name:string -> t
end
