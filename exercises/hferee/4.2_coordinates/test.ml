
open Test_lib
open Report

let exercise =
  []

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

