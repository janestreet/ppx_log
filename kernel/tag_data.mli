open! Base
open! Import
open! Ppxlib

(** Represents the data of a tag in a statement. For example, in

    {|[%log.global "message" ~label1:"tag1" ~label2:(tag2 : int) ~label3:tag3 [%here]]|}

    ["tag1"] would be a [Constant]
    [(tag2 : int)] would be [Type_constrained]
    [tag3] would be a [String_expression]
    [%here] would be a [Here_extension]
*)
type t =
  | Constant of constant
  | Type_constrained of expression * core_type
  | String_expression of expression
  | Here_extension
[@@deriving sexp_of]

val parse : expression -> t loc

(** Renders the tag into an expression either of type [Log_tag.t] or type [Log_tag.t
    option], depending on the attributes (@sexp.option and @sexp.omit_nil).

    Example output: [Int 123] for constant tags, [Sexp ([%sexp_of: typ] expr)] for types
    that aren’t specially handled *)
val render : t loc -> [ `Tag | `Tag_option ] * expression
