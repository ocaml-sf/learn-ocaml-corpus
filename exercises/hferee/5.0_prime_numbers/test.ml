
open Test_lib
open Report

let testint name =
   Section ([ Code name ], test_variable_against_solution [%ty : int] name)

let rec ints n = if n <= 0 then [0] else n :: ints (n - 1)


let exercise =
  [
    Section (
      [ Code "div" ],
      test_function_2_against_solution [%ty : int -> int -> bool] "div"
        ~gen:15 [(1, 0); (1, 2); (2, 1); (4, 2); (2, 4); (51, 5)]
        ~sampler:(fun () -> 1 + abs(sample_int()), abs(sample_int()))
    );
    Section (
      [ Code "dividers" ],
      test_function_2_against_solution [%ty : int -> int -> int] "dividers"
        ~sampler:(fun () -> 1 + abs(sample_int()), abs(sample_int()))
        ~gen:15 [(1, 0); (1, 2); (2, 1); (4, 2); (2, 4);
                 (51, 5); (6, 3); (25, 6); (25, 5); (25, 4)]
    );
    Section (
      [ Code "prime" ],
      test_function_1_against_solution [%ty : int -> bool] "prime"
        ~gen:0 (ints 100)
    );

  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

