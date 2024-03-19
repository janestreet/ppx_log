open! Core

let%expect_test "Extension names" =
  Ppx_log_kernel.For_testing.extension_names
  |> List.sort ~compare:[%compare: string]
  |> List.iter ~f:print_endline;
  [%expect
    {|
    @log
    @log.debug
    @log.debug_format
    @log.debug_sexp
    @log.debug_string
    @log.error
    @log.error_format
    @log.error_sexp
    @log.error_string
    @log.format
    @log.global
    @log.global.debug
    @log.global.debug_format
    @log.global.debug_sexp
    @log.global.debug_string
    @log.global.error
    @log.global.error_format
    @log.global.error_sexp
    @log.global.error_string
    @log.global.format
    @log.global.info
    @log.global.info_format
    @log.global.info_sexp
    @log.global.info_string
    @log.global.sexp
    @log.global.string
    @log.info
    @log.info_format
    @log.info_sexp
    @log.info_string
    @log.sexp
    @log.string
    |}]
;;
