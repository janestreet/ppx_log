open! Base
open! Import

module Level = struct
  type t =
    [ `Debug
    | `Info
    | `Error
    ]
  [@@deriving enumerate]

  let to_expression t ~loc =
    match t with
    | `Debug -> [%expr `Debug]
    | `Info -> [%expr `Info]
    | `Error -> [%expr `Error]
  ;;

  let to_string = function
    | `Debug -> "debug"
    | `Info -> "info"
    | `Error -> "error"
  ;;
end

module Format = struct
  type t =
    [ `String
    | `Message
    | `Sexp
    | `Printf
    ]
  [@@deriving enumerate]

  let extension_suffix = function
    | `Message -> None
    | `Sexp -> Some "sexp"
    | `Printf -> Some "format"
    | `String -> Some "string"
  ;;
end

type t =
  { level : Level.t option
  ; log_kind : unit Log_kind.t
  ; format : Format.t
  }
[@@deriving enumerate]

let name { format; level; log_kind } =
  let level_str = Option.map level ~f:Level.to_string in
  let format_suffix = Format.extension_suffix format in
  let extension_prefix = Log_kind.extension_prefix log_kind in
  match level_str, format_suffix with
  | None, None -> extension_prefix
  | None, Some suffix -> extension_prefix ^ "." ^ suffix
  | Some level, None -> extension_prefix ^ "." ^ level
  | Some level, Some suffix -> extension_prefix ^ "." ^ level ^ "_" ^ suffix
;;
