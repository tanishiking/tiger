open OUnit2
open BasicSemanticSuite
open SemanticErrorSuite

let () =
  run_test_tt_main basic_suite;
  run_test_tt_main semantic_error_suite
