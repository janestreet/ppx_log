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

let ext kind =
  let name = Extension_kind.name kind in
  Extension.declare name Extension.Context.expression pattern (expand kind)
;;

let extensions = List.map Extension_kind.all ~f:ext
let () = Driver.register_transformation "log" ~extensions

module Log_tag = Log_tag
module Tag_data = Tag_data

module For_testing = struct
  let extension_names = List.map Extension_kind.all ~f:Extension_kind.name
end
