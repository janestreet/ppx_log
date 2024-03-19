open! Base
open! Import
open! Ppxlib_with_sexp

type t =
  { label : label
  ; data : Tag_data.t loc
  }
[@@deriving fields ~getters]

let compare_label = Comparable.lift [%compare: string] ~f:label
let sexp_of_t { label; data } = [%sexp ((label, data.txt) : string * Tag_data.t)]
let inferred_label = Pprintast.string_of_expression

let parse_arg (label, e) =
  let loc = { e.pexp_loc with loc_ghost = true } in
  let label =
    match label, e.pexp_desc with
    | Nolabel, Pexp_constraint (expr, (_ : core_type)) -> inferred_label expr
    | Nolabel, (_ : expression_desc) | Labelled "_", (_ : expression_desc) -> ""
    | Labelled label, (_ : expression_desc) -> label
    | Optional (_ : label), (_ : expression_desc) ->
      Location.raise_errorf ~loc "optional argument not allowed here"
  in
  { label; data = Tag_data.parse e }
;;

let render_list ts ~loc =
  (* We have to render the whole list of [Log_tag.t]s into one expression, vs. creating
     a list of expressions, because that avoids us having to do some kind of
     [List.filter_opt] in the generated code. *)
  List.fold (List.rev ts) ~init:[%expr []] ~f:(fun acc { label; data } ->
    let label = Ast_builder.Default.estring label ~loc:data.loc in
    match Tag_data.render data with
    | `Tag, data ->
      [%expr { Ppx_log_types.Log_tag.name = [%e label]; data = [%e data] } :: [%e acc]]
    | `Tag_option, data ->
      [%expr
        match [%e data], [%e acc] with
        | None, tl -> tl
        | Some data, tl -> { Ppx_log_types.Log_tag.name = [%e label]; data } :: tl])
;;

let%expect_test "parsing / rendering examples" =
  let loc = Location.none in
  let test e =
    Ast_pattern.(parse (pexp_apply drop __)) loc e (fun args ->
      match Or_error.try_with (fun () -> List.map args ~f:parse_arg) with
      | Ok ts ->
        print_s [%sexp (ts : t list)];
        Pprintast.string_of_expression (render_list ts ~loc) |> print_endline
      | Error e -> print_s [%sexp (e : Error.t)])
  in
  test [%expr "unused" ~x:123];
  [%expect
    {|
    ((x (Constant (Pconst_integer 123 ()))))
    [{ Ppx_log_types.Log_tag.name = "x"; data = (Int 123) }]
    |}];
  test [%expr "unused" ~x:(some ~expr)];
  [%expect
    {|
    ((x (String_expression "some ~expr")))
    [{ Ppx_log_types.Log_tag.name = "x"; data = (String (some ~expr)) }]
    |}];
  test [%expr "unused" ~x:(some ~expr : t)];
  [%expect
    {|
    ((x (Type_constrained "some ~expr" t)))
    [{
       Ppx_log_types.Log_tag.name = "x";
       data = (Sexp (((sexp_of_t)[@merlin.hide ]) (some ~expr)))
     }]
    |}];
  test [%expr "unused" ~x];
  [%expect
    {|
    ((x (String_expression x)))
    [{ Ppx_log_types.Log_tag.name = "x"; data = (String x) }]
    |}];
  test [%expr "unused" (some ~expr : t)];
  [%expect
    {|
    (("some ~expr" (Type_constrained "some ~expr" t)))
    [{
       Ppx_log_types.Log_tag.name = "some ~expr";
       data = (Sexp (((sexp_of_t)[@merlin.hide ]) (some ~expr)))
     }]
    |}];
  test [%expr "unused" (some ~expr)];
  [%expect
    {|
    (("" (String_expression "some ~expr")))
    [{ Ppx_log_types.Log_tag.name = ""; data = (String (some ~expr)) }]
    |}];
  test [%expr "unused" ~_:(some ~expr)];
  [%expect
    {|
    (("" (String_expression "some ~expr")))
    [{ Ppx_log_types.Log_tag.name = ""; data = (String (some ~expr)) }]
    |}];
  test [%expr "unused" ~_:(some ~expr : t)];
  [%expect
    {|
    (("" (Type_constrained "some ~expr" t)))
    [{
       Ppx_log_types.Log_tag.name = "";
       data = (Sexp (((sexp_of_t)[@merlin.hide ]) (some ~expr)))
     }]
    |}];
  test
    [%expr
      "unused" ~x ~y (z : (int option[@sexp.option])) (t : (t[@sexp.omit_nil])) ~a ~b];
  [%expect
    {|
    ((x (String_expression x)) (y (String_expression y))
     (z (Type_constrained z "((int option)[@sexp.option ])"))
     (t (Type_constrained t "((t)[@sexp.omit_nil ])")) (a (String_expression a))
     (b (String_expression b)))
    { Ppx_log_types.Log_tag.name = "x"; data = (String x) } ::
    { Ppx_log_types.Log_tag.name = "y"; data = (String y) } ::
    (match ((match z with
             | None -> None
             | Some value ->
                 Some
                   (Ppx_log_types.Tag_data.Sexp
                      (((sexp_of_int)[@merlin.hide ]) value))),
             (match ((match ((sexp_of_t)[@merlin.hide ]) t with
                      | Sexp.List [] -> None
                      | sexp -> Some (Ppx_log_types.Tag_data.Sexp sexp)),
                      [{ Ppx_log_types.Log_tag.name = "a"; data = (String a) };
                      { Ppx_log_types.Log_tag.name = "b"; data = (String b) }])
              with
              | (None, tl) -> tl
              | (Some data, tl) -> { Ppx_log_types.Log_tag.name = "t"; data } ::
                  tl))
     with
     | (None, tl) -> tl
     | (Some data, tl) -> { Ppx_log_types.Log_tag.name = "z"; data } :: tl)
    |}]
;;
