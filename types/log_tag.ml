open! Base
open! Import

type t =
  { name : string
  ; data : Tag_data.t
  }

let of_pair (name, data) = { name; data }
let string_pair (name, data) = { name; data = String data }

module Verbose = struct
  type nonrec t = t =
    { name : string
    ; data : Tag_data.With_type_label.t
    }
  [@@deriving sexp_of]
end

module For_message_sexp = struct
  type nonrec t = t

  let sexp_of_t = function
    | { name = ""; data } -> [%sexp (data : Tag_data.Without_type_label.t)]
    | { name; data } -> [%sexp (name : string), (data : Tag_data.Without_type_label.t)]
  ;;
end
