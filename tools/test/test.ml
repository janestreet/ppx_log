open! Core
open! Async

let compare ~format =
  Log.Global.set_output [ Log.Output.stdout () ~format ];
  Log.Global.set_level `Debug;
  print_endline "Input before refactor";
  let%bind () = Input.do_log () in
  let input_logs = Expect_test_helpers_base.expect_test_output [%here] in
  print_endline "Output after refactor";
  let%map () = Output.do_log () in
  let output_logs = Expect_test_helpers_base.expect_test_output [%here] in
  Expect_test_patdiff.print_patdiff
    input_logs
    output_logs
    ~context:(String.length (input_logs ^ output_logs))
      (* big overestimate for context but that’s fine *)
    ~location_style:None
;;

let%expect_test "compare input and output log statement behaviour" =
  let%bind () = compare ~format:`Text in
  [%expect
    {|
    -|Input before refactor
    +|Output after refactor
      1969-12-31 19:00:00.000000-05:00 what's up /world\
      1969-12-31 19:00:00.000000-05:00 /world\
      1969-12-31 19:00:00.000000-05:00
      1969-12-31 19:00:00.000000-05:00 Info ()
      1969-12-31 19:00:00.000000-05:00 Info error
      1969-12-31 19:00:00.000000-05:00 Info (host"my host")
      1969-12-31 19:00:00.000000-05:00 Info (host"my host")
      1969-12-31 19:00:00.000000-05:00 Info ((host"my host")(e"my error"))
      1969-12-31 19:00:00.000000-05:00 Info ((host"my host")(e"my error")) -- [hello2: world]
      1969-12-31 19:00:00.000000-05:00 Info (message3"my error")
      1969-12-31 19:00:00.000000-05:00 Info (message4(i 5))
      1969-12-31 19:00:00.000000-05:00 Info (message5(i(5)))
      1969-12-31 19:00:00.000000-05:00 Info ("label pun 1"(host"my host"))
      1969-12-31 19:00:00.000000-05:00 Info ("label pun 2"(host"my host"))
      1969-12-31 19:00:00.000000-05:00 Info ("label pun 3"(host"my host"))
      1969-12-31 19:00:00.000000-05:00 Info ("labelled 4"(my_host"my host"))
      1969-12-31 19:00:00.000000-05:00 Info "my error"
      1969-12-31 19:00:00.000000-05:00 Info "my error"
      1969-12-31 19:00:00.000000-05:00 Info (message8"my host"(e"my error"))
      1969-12-31 19:00:00.000000-05:00 Info ("my message""my host")
      1969-12-31 19:00:00.000000-05:00 Info ("my message"((host"my host")))
      1969-12-31 19:00:00.000000-05:00 Info ("hello11 5"((host"my host")))
      1969-12-31 19:00:00.000000-05:00 Debug hello12 -- [hello: world]
      1969-12-31 19:00:00.000000-05:00 Debug hello13 -- [hello: world]
      1969-12-31 19:00:00.000000-05:00 hello world14
      1969-12-31 19:00:00.000000-05:00 hello world15 5
      1969-12-31 19:00:00.000000-05:00 Info error
      1969-12-31 19:00:00.000000-05:00 Info ((host"my host")(e"my error")) -- [hello17: world]
      1969-12-31 19:00:00.000000-05:00 Debug hello
      1969-12-31 19:00:00.000000-05:00 Debug partial application of format 18
      1969-12-31 19:00:00.000000-05:00 Debug partial application of format 2
      1969-12-31 19:00:00.000000-05:00 Debug partial application of format 3
      1969-12-31 19:00:00.000000-05:00 custom printf 3 4
      1969-12-31 19:00:00.000000-05:00 custom printf but with regular format arg 3
      1969-12-31 19:00:00.000000-05:00 no args in custom printf %
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
      1969-12-31 19:00:00.000000-05:00 Info Hello
      1969-12-31 19:00:00.000000-05:00 Debug hello29 -- [hello: world]
      1969-12-31 19:00:00.000000-05:00 Debug hello30 -- [hello: world]
      1969-12-31 19:00:00.000000-05:00 hello world31
      1969-12-31 19:00:00.000000-05:00 hello world32 5
      1969-12-31 19:00:00.000000-05:00 "direct hello"
      1969-12-31 19:00:00.000000-05:00 Info direct hello 2
    |}];
  let%bind () = compare ~format:`Sexp in
  [%expect
    {|
    -|Input before refactor
    +|Output after refactor
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"what's up /world\\"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"/world\\"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String""))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp()))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp error))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(host"my host")))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(host"my host")))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp((host"my host")(e"my error"))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp((host"my host")(e"my error"))))(tags((hello2 world)))))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(message3"my error")))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(message4(i 5))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(message5(i(5)))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("label pun 1"(host"my host"))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("label pun 2"(host"my host"))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("label pun 3"(host"my host"))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("labelled 4"(my_host"my host"))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp"my error"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp"my error"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(message8"my host"(e"my error"))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("my message""my host")))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("my message"((host"my host")))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("hello11 5"((host"my host")))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Debug))(message(Sexp hello12))(tags((hello world)))))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Debug))(message(Sexp hello13))(tags((hello world)))))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"hello world14"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"hello world15 5"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp error))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp((host"my host")(e"my error"))))(tags((hello17 world)))))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Debug))(message(String hello))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Debug))(message(String"partial application of format 18"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Debug))(message(String"partial application of format 2"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Debug))(message(String"partial application of format 3"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"custom printf 3 4"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"custom printf but with regular format arg 3"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"no args in custom printf %"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(message18"my error")))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(message19(i 5))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(message20(i(5)))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp"my error"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp hello))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp"my error"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp(message24"my host"(e"my error"))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("my message""my host")))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp("my message"((host"my host")))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp((a 123))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp((j(5)))))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(Sexp Hello))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Debug))(message(Sexp hello29))(tags((hello world)))))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Debug))(message(Sexp hello30))(tags((hello world)))))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"hello world31"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(String"hello world32 5"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level())(message(Sexp"direct hello"))(tags())))
      (V2((time(1969-12-31 19:00:00.000000-05:00))(level(Info))(message(String"direct hello 2"))(tags())))
    |}];
  Deferred.unit
;;
