open OUnit2
open Util

let semantic_error_suite =
  "SemanticError"
  >::: [
         semantic_error_test "basix_undefined_variable" "x"
           "undefined variable x";
         semantic_error_test "undefined_variable_let" "let var x := 1 in y end"
           "undefined variable y";
         semantic_error_test "vardec type mismatch"
           "let var x : string := 1 in x end" "type mismatch";
         semantic_error_test "vardec type mismatch" "let var x := nil in x end"
           "cannot initialize";
       ]
