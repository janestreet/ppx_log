open! Base
open! Import
include Ppxlib

let sexp_as_pprinted_atom printer data =
  (* This [str_formatter] work roughly follows [Pprintast.string_of_expression]. *)
  ignore (Stdlib.Format.flush_str_formatter () : string);
  printer Stdlib.Format.str_formatter data;
  Sexp.Atom (Stdlib.Format.flush_str_formatter ())
;;

let sexp_of_constant = Ast_traverse.sexp_of#constant

(* Locations are remarkably verbose, so the sexp printout pretty-prints to avoid them. *)
let sexp_of_expression = sexp_as_pprinted_atom Pprintast.expression
let sexp_of_core_type = sexp_as_pprinted_atom Pprintast.core_type
let sexp_of_loc sexp_of_a { loc = (_ : location); txt } = sexp_of_a txt
