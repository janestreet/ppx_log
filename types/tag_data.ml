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
[@@deriving sexp_of]

module With_type_label = struct
  type nonrec t = t [@@deriving sexp_of]
end

module Without_type_label = struct
  type nonrec t = t

  let sexp_of_t = function
    | Int x -> [%sexp_of: int] x
    | Char x -> [%sexp_of: char] x
    | Float x -> [%sexp_of: float] x
    | String x -> [%sexp_of: string] x
    | Bool x -> [%sexp_of: bool] x
    | Sexp x -> x
    | Json x -> [%sexp_of: Jsonaf.t] x
  ;;
end
