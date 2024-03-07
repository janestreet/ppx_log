open! Base

type t = Nothing.t
type time = Nothing.t
type return_type = [ `Ppx_log_syntax_implementation_missing ]

let would_log = Nothing.unreachable_code
let default = `Ppx_log_syntax_implementation_missing
let message ?level:_ ?time:_ ?tags:_ = Nothing.unreachable_code

module Global = struct
  let would_log = Fn.const false
  let default = default
  let message ?level:_ ?time:_ ?tags:_ _ _ = default
end
