open! Base
open Ppxlib

let log_source_position = ref false

let () =
  Driver.add_arg
    "-log-source-position"
    (Set log_source_position)
    ~doc:
      " If set, adds a \"pos\" tag with a source code position to every logged message."
;;

let pattern = Parsed_extension.pattern ()

let expand ~loc ~path:(_ : label) kind parsed =
  let log_source_position = !log_source_position in
  Log_statement.create kind parsed ~loc ~log_source_position |> Log_statement.render
;;

let correct_and_expand
  ~(loc : location)
  ~path
  ~extension_name
  ~corrected_name
  ~kind
  (parsed : Parsed_extension.t)
  =
  (* The ppx [Extension.declare] interface doesn’t give us access to the ‘outer’ payload
     except for its location. So loc covers:
     {v
         [%log.... ...]
          ^......^
          1      1+name length
     v}
  *)
  let corrected_name = String.tr ~target:'@' ~replacement:'%' corrected_name in
  let add_cols (pos : Lexing.position) n = { pos with pos_cnum = pos.pos_cnum + n } in
  Driver.register_correction
    ~loc:
      { loc_start = add_cols loc.loc_start 1
      ; loc_end = add_cols loc.loc_start (1 + String.length extension_name)
      ; loc_ghost = loc.loc_ghost
      }
    ~repl:corrected_name;
  expand ~loc ~path kind parsed
;;

let ext kind =
  let name = Extension_kind.name kind in
  match kind.log_kind with
  | `Global | `Instance () ->
    Extension.declare name Extension.Context.expression pattern (expand kind)
  | `Explicit_global ->
    let corrected_name = Extension_kind.name { kind with log_kind = `Global } in
    Extension.declare
      name
      Extension.Context.expression
      pattern
      (correct_and_expand ~extension_name:name ~corrected_name ~kind)
;;

let ext_raw_message =
  Extension.declare
    "log.make_raw"
    Extension.Context.expression
    (Raw_message.pattern ())
    (fun ~loc:(_ : location) ~path:(_ : label) -> Raw_message.render)
;;

let extensions = ext_raw_message :: List.map Extension_kind.all ~f:ext
let () = Driver.register_transformation "log" ~extensions

module Log_tag = Log_tag
module Tag_data = Tag_data

module For_testing = struct
  let extension_names = List.map Extension_kind.all ~f:Extension_kind.name
end
