open! Base
open! Import

module Code_position = struct
  type t =
    { pos_fname : string
    ; pos_lnum : int
    ; library_name : string
    }
  [@@deriving sexp_of]
end

type t =
  | Manually_constructed of string
  | Code of Code_position.t
[@@deriving sexp_of]

module Private = struct
  let libname ~module_name =
    match String.substr_index module_name ~pattern:"__" with
    | Some idx -> String.sub module_name ~pos:0 ~len:idx
    | None -> module_name
  ;;

  let%expect_test "libname_from_module_name example" =
    print_string (libname ~module_name:Stdlib.__MODULE__);
    [%expect "Ppx_log_types"];
    print_string (libname ~module_name:"Lib_name");
    [%expect "Lib_name"];
    print_string (libname ~module_name:"Lib_name2__module_name");
    [%expect "Lib_name2"];
    print_string (libname ~module_name:"Lib_name3__module_name__more_stuff");
    [%expect "Lib_name3"]
  ;;

  let code ~pos_fname ~pos_lnum ~module_name =
    Code { pos_fname; pos_lnum; library_name = libname ~module_name }
  ;;
end
