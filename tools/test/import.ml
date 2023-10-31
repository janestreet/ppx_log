(* Intentionally shadow [Log] for test. *)
module Log = struct
  let sexp _ _ ~test:_ = ()
end
