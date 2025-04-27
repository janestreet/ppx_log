open! Base
open! Import

type t = Message_source.t * Message_data.t

let sexp_of_t_hum ((source, data) : t) =
  let source =
    match source with
    | Manually_constructed loc -> loc
    | Code { pos_fname; pos_lnum; library_name } ->
      pos_fname ^ ":" ^ Int.to_string pos_lnum ^ " (" ^ library_name ^ ")"
  in
  let data = Message_data.sexp_of_t_hum data in
  [%sexp (data : Sexp.t), { source : string }]
;;
