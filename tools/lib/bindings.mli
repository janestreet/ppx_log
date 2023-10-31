open! Core
include module type of Refactor_syntax.Bindings

(** Gets the final bindings (i.e., mapping of identifiers to user-specified data)
    exported by an ML file. *)
val process_ml_file_exn : 'a t -> Refactor_editor.t -> 'a t
