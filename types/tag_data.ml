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
[@@deriving sexp_of ~stackify, variants]

module With_type_label = struct
  type nonrec t = t [@@deriving sexp_of ~stackify]
end

module Without_type_label = struct
  type nonrec t = t

  let%template[@alloc a = (heap, stack)] sexp_of_t t =
    match[@exclave_if_stack a] t with
    | Int x -> ([%sexp_of: int] [@alloc a]) x
    | Char x -> ([%sexp_of: char] [@alloc a]) x
    | Float x -> ([%sexp_of: float] [@alloc a]) x
    | String x -> ([%sexp_of: string] [@alloc a]) x
    | Bool x -> ([%sexp_of: bool] [@alloc a]) x
    | Sexp x -> x
    | Json x -> ([%sexp_of: Jsonaf.t] [@alloc a]) x
  ;;

  let to_string = function
    | Int x -> Int.to_string x
    | Char x -> Char.to_string x
    | Float x -> Float.to_string x
    | String x -> x
    | Bool x -> Bool.to_string x
    | Sexp x -> Sexp.to_string x
    | Json x -> Sexp.to_string ([%sexp_of: Jsonaf.t] x)
  ;;
end
