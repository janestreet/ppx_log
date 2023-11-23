open! Base
open! Import
include module type of Ppxlib

val sexp_of_constant : constant -> Sexp.t
val sexp_of_expression : expression -> Sexp.t
val sexp_of_core_type : core_type -> Sexp.t
val sexp_of_loc : ('a -> Sexp.t) -> 'a loc -> Sexp.t
