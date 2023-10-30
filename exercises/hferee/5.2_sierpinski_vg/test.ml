
open Test_lib
open Report

let exercise =
  [
    Section(
      [Code "one_triangle"],
      test_variable_against_solution [%ty: image] "one_triangle");
    Section(
      [Code "two_triangles"],
      test_variable_against_solution [%ty: image] "two_triangles");
    Section(
      [Code "sierpinsky"],
      test_function_1_against_solution [%ty: int -> image] "sierpinsky" ~gen:0
        [0; 1; 2; 5]
    );
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

