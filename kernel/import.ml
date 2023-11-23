(* [Stdio] is typically included in [Core], but we can't depend on [Core] here because
   [Core] uses [ppx_jane]. *)
include Stdio
