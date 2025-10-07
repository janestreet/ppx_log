open! Base

type time = Nothing.t

module Instance = struct
  type t = Nothing.t
  type return_type = [ `Ppx_log_syntax_implementation_missing ]

  let would_log = Nothing.unreachable_code
  let default = `Ppx_log_syntax_implementation_missing
  let message ?level:_ ?time:_ ?tags:_ = Nothing.unreachable_code
end

module Global = struct
  type return_type = [ `Ppx_log_syntax_implementation_missing ]

  let would_log _ = false
  let default = Instance.default
  let message ?level:_ ?time:_ ?tags:_ _ _ = default
end
