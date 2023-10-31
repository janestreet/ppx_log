open! Core
open! Async

type ('key, 'data) t =
  { cache_dir : File_path.t
  ; key_to_filename : 'key -> File_path.Part.t
  ; data_format : (module Sexpable.S with type t = 'data)
  ; compute : 'key -> 'data Deferred.t
  }

let create ~key_to_filename ~data_format ~compute ~cache_dir =
  { key_to_filename; data_format; compute; cache_dir }
;;

let fetch
  (type data)
  { cache_dir
  ; data_format = (module Data : Sexpable.S with type t = data)
  ; key_to_filename
  ; compute
  }
  key
  =
  let cached_path = File_path.append_part cache_dir (key_to_filename key) in
  if%bind Filesystem_async.exists_exn cached_path
  then Filesystem_async.load_as_sexp cached_path ~of_sexp:[%of_sexp: Data.t]
  else (
    let lock_path =
      File_path.append_to_basename_exn cached_path ".lock" |> File_path.to_string
    in
    match%bind Lock_file_async.Flock.lock_exn () ~lock_path with
    | `We_took_it lock ->
      let%bind data = compute key in
      let%bind () = Filesystem_async.save_sexp cached_path [%sexp (data : Data.t)] in
      let%map () = Lock_file_async.Flock.unlock_exn lock in
      data
    | `Somebody_else_took_it ->
      let%bind lock = Lock_file_async.Flock.wait_for_lock_exn () ~lock_path in
      let%bind () = Lock_file_async.Flock.unlock_exn lock in
      Filesystem_async.load_as_sexp cached_path ~of_sexp:[%of_sexp: Data.t])
;;
