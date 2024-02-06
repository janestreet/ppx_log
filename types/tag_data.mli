open! Base
open! Import

type t =
  | Int of int
  | Char of char
  | Float of float
  | String of string
  | Bool of bool
  | Sexp of Sexp.t
  | Json of Jsonaf.t

module Without_type_label : sig
  type nonrec t = t [@@deriving sexp_of]

  val to_string : t -> string
end

module With_type_label : sig
  type nonrec t = t [@@deriving sexp_of]
end
