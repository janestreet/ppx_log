open! Base
open! Import
open! Ppxlib

type t = { loc : location }

val render : t -> expression
