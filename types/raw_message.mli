open! Base
open! Import

(** This type can be created with ppx [%log.make_raw].

    You can log this type with any [raw] ppx log extension, e.g. [%log.global.error_raw]

    To pull out any specific information about your raw message, refer to the
    corresponding modules [Message_source] and [Message_data].

    Use this type when you want to create a logable message while also preserving
    structural information. *)
type t = Message_source.t * Message_data.t

(** It's common to want to create a raw message, and also embed it as a sexp in downstream
    info types like errors. This helper function provides this. *)
val sexp_of_t_hum : t -> Sexp.t
