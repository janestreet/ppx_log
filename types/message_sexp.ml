open! Base
open! Import

module Label = struct
  type t =
    | String of string
    | String_literal of string
  [@@deriving sexp_of]
end

type t =
  { label : Label.t option
  ; tags : Log_tag.t list
  ; legacy_render_with_additional_parentheses : bool
  }
[@@deriving fields ~getters ~iterators:create]

let create ?(legacy_render_with_additional_parentheses = false) label ~tags =
  { label; tags; legacy_render_with_additional_parentheses }
;;

let render { label; tags; legacy_render_with_additional_parentheses } =
  (* For compatibility with [%sexp "message", { a1 : t1; a2 : t2 }] producing sexps
     where the tags are in an extra layer of parentheses (message ((a1 .) (a2 .))), we
     have extra logic for [legacy_render_with_additional_parentheses].

     [%message] has some magic in 2 cases:
     - "" = Nothing at all. So:
       [%message "hello" ~i:(3 : int) ~j:(3 : int)] -> (hello (i 3) (j 3))
       [%message "" ~i:(3 : int) ~j:(3 : int)] -> ((i 3) (j 3))
     - If the final output has format (<one-elt>), the outer parentheses are removed.
       [%message "hello"] -> hello, NOT (hello)
       [%message "" ~i:(3 : int)] -> (i 3), NOT ((i 3))
     - However, [%message ""] = () = Sexp.List []. *)
  let label =
    Option.map label ~f:(fun (String str | String_literal str) -> Sexp.Atom str)
  in
  let tags = List.map tags ~f:[%sexp_of: Log_tag.For_message_sexp.t] in
  match label, tags with
  | None, [] -> Sexp.List []
  | Some sexp, [] -> sexp
  | None, [ tag ] ->
    if legacy_render_with_additional_parentheses then List [ tag ] else tag
  | None, multiple_tags -> List multiple_tags
  | Some message, multiple_tags ->
    if legacy_render_with_additional_parentheses
    then List [ message; List multiple_tags ]
    else List (message :: multiple_tags)
;;

module Unstable = struct
  type nonrec t = t =
    { label : Label.t option [@sexp.option]
    ; tags : Log_tag.Verbose.t list
    ; legacy_render_with_additional_parentheses : bool [@sexp.bool]
    }
  [@@deriving sexp_of]
end
