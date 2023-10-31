(** A module that runs computations based on [key] and caches the results on the
    filesystem so that other processes with the same cache handle can access the results
    without recomputing. *)

open! Core
open! Async

type ('key, 'data) t

val create
  :  key_to_filename:('key -> File_path.Part.t)
  -> data_format:(module Sexpable.S with type t = 'data)
  -> compute:('key -> 'data Deferred.t)
  -> cache_dir:File_path.t
  -> ('key, 'data) t

val fetch : ('key, 'data) t -> 'key -> 'data Deferred.t
