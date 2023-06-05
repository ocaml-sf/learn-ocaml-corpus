
open Test_lib
open Report

let exercise =
  [
    Section(
      [Code "move"],
        test_function_2_against_solution [%ty: tower -> tower -> unit] "move"
          ~test:test_ignore ~gen:0
          ~test_stdout:io_test_lines [L, M; L, R; M, R; M, L; R, L; R, M]
    );
    Section(
      [Code "tower3"],
      test_function_1_against_solution [%ty: unit -> unit] "tower3"
        ~test:test_ignore ~gen:0
        ~test_stdout:io_test_lines [()]
    );
    Section(
      [Code "solve_tower"],
      test_function_1_against_solution [%ty: int -> unit] "solve_tower"
        ~test:test_ignore ~gen:0
        ~test_stdout:io_test_lines [0; 1; 2; 3; 4; 5]
    );
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

