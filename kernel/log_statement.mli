open! Base
open! Import
open Ppxlib

type t

val create
  :  Extension_kind.t
  -> Parsed_extension.t
  -> loc:location
  -> log_source_position:bool
  -> t

val render : t -> expression
