open OUnit2
open ParseBinopSuite
open ParseLetExpSuite
open BasicParserSuite

let () =
  run_test_tt_main basic_suite;
  run_test_tt_main parse_binop_suite;
  run_test_tt_main parse_letexp_suite
