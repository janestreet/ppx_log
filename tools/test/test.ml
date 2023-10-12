open! Core
open! Async

let%expect_test "compare input and output log statement behaviour" =
  print_endline "Input before refactor";
  let%bind () = Input.do_log () in
  let input_logs = [%expect.output] in
  print_endline "Output after refactor";
  let%map () = Output.do_log () in
  let output_logs = [%expect.output] in
  Expect_test_patdiff.print_patdiff
    input_logs
    output_logs
    ~context:(String.length (input_logs ^ output_logs))
      (* big overestimate for context but that’s fine *)
    ~location_style:None;
  [%expect
    {|
    -|Input before refactor
    +|Output after refactor
      1969-12-31 19:00:00.000000-05:00 Info error
      1969-12-31 19:00:00.000000-05:00 Info ((host"my host")(e"my error")) -- [hello2: world]
      1969-12-31 19:00:00.000000-05:00 Info (message3"my error")
      1969-12-31 19:00:00.000000-05:00 Info (message4(i 5))
      1969-12-31 19:00:00.000000-05:00 Info (message5(i(5)))
      1969-12-31 19:00:00.000000-05:00 Info "my error"
      1969-12-31 19:00:00.000000-05:00 Info "my error"
      1969-12-31 19:00:00.000000-05:00 Info (message8"my host"(e"my error"))
      1969-12-31 19:00:00.000000-05:00 Info ("my message""my host")
      1969-12-31 19:00:00.000000-05:00 Info ("my message"((host"my host")))
      1969-12-31 19:00:00.000000-05:00 Info ("hello11 5"((host"my host")))
      1969-12-31 19:00:00.000000-05:00 hello world14
      1969-12-31 19:00:00.000000-05:00 hello world15 5
      1969-12-31 19:00:00.000000-05:00 Info error
      1969-12-31 19:00:00.000000-05:00 Info ((host"my host")(e"my error")) -- [hello17: world]
      1969-12-31 19:00:00.000000-05:00 Info (message18"my error")
      1969-12-31 19:00:00.000000-05:00 Info (message19(i 5))
      1969-12-31 19:00:00.000000-05:00 Info (message20(i(5)))
      1969-12-31 19:00:00.000000-05:00 Info "my error"
      1969-12-31 19:00:00.000000-05:00 Info hello
      1969-12-31 19:00:00.000000-05:00 Info "my error"
      1969-12-31 19:00:00.000000-05:00 Info (message24"my host"(e"my error"))
      1969-12-31 19:00:00.000000-05:00 Info ("my message""my host")
      1969-12-31 19:00:00.000000-05:00 Info ("my message"((host"my host")))
      1969-12-31 19:00:00.000000-05:00 Info ((a 123))
      1969-12-31 19:00:00.000000-05:00 Info ((j(5)))
      1969-12-31 19:00:00.000000-05:00 hello world31
      1969-12-31 19:00:00.000000-05:00 hello world32 5 |}]
;;
