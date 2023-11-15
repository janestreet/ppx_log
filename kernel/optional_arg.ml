open! Base
open! Import
open Ppxlib

type t = [ `Labelled of expression | `Optional of expression ] option

let of_optional_expr = Option.map ~f:(fun e -> `Optional e)

let to_expr t ~loc =
  match t with
  | None -> [%expr None]
  | Some (`Optional v) -> v
  | Some (`Labelled v) -> [%expr Some [%e v]]
;;

let to_arg t ~name =
  Option.map t ~f:(function
    | `Optional expr -> Optional name, expr
    | `Labelled expr -> Labelled name, expr)
;;
