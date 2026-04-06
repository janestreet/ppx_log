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

  let%template[@alloc a = (heap, stack)] sexp_of_t t =
    match[@exclave_if_stack a] t with
    | { name = ""; data } -> [%sexp (data : Tag_data.Without_type_label.t)] [@alloc a]
    | { name; data } ->
      [%sexp (name : string), (data : Tag_data.Without_type_label.t)] [@alloc a]
  ;;
end
