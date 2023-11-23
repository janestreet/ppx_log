open! Base
open! Import
open! Ppxlib

type t =
  { label : label
  ; data : Tag_data.t loc
  }
[@@deriving fields ~getters]

(** order by the [label]s *)
val compare_label : t -> t -> int

(** See tests in the implementation for more examples.

    Parsing rules:
    {v
      ~x:123         => {label = "x"; data = Constant <123>}
      ~x:some_expr   => {label = "x"; data = String_expression some_expr}
      ~x             => {label = "x"; data = String_expression x}
      ~x:(expr : t)  => {label = "x"; data = Type_constrained (expr, t)}
      (expr : t)     => {label = to_string expr; data = Type_constrained (expr, t)}

      expr           => {label = ""; data = String_expression expr}
      ~_:expr        => {label = ""; data = String_expression expr}
      ~_:(expr : t)  => {label = ""; data = Type_constrained (expr, t)}
      ?x:expr        => invalid
   v}
*)
val parse_arg : arg_label * expression -> t

(** Produces an expression that will evaluate to a [Log_tag.t list]. *)
val render_list : t list -> loc:location -> expression
