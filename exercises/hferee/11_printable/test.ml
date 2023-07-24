
open Test_lib
open Report

let testint name =
   Section ([ Code name ], test_variable_against_solution [%ty : int] name)


let exercise =
  [
    (*
    Section(
      [Text "Question 1"],
      test_function_2_against_solution [%ty : (int show) -> int -> unit]
        ~test_stdout:io_test_equals ~gen:0 "print"
        [Show print_int, -42] @
      test_function_2_against_solution [%ty : ((int * int) show) -> (int * int) -> unit]
        ~test_stdout:io_test_equals ~gen:0 "print"
        [test_show, (4, 2); test_show (-4, 2)]
    ); *)
    Section(
      [Text "No automatic correction for this exercise"], [])
   ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

