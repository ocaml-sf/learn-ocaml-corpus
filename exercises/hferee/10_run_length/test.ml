
open Test_lib
open Report

let exercise =
  [
    Section(
      [Code "encode"],
      test_function_1_against_solution [%ty: int list -> (int * int) list]
        ~gen:0 "encode" [[1; 1; 1; 2; 0; 1]] @
      test_function_1_against_solution [%ty: float list -> (float * int) list]
        ~gen:0 "encode" [[]]);

    Section(
      [Code "decode"],
      test_function_1_against_solution [%ty: (int * int) list -> int list]
        ~gen:0 "decode" [[1, 3; 2, 1; 3, 2]] @
      test_function_1_against_solution [%ty: (float * int) list -> float list]
        ~gen:0 "decode" [[]]);

    Section(
      [Code "mystery"],
      test_function_1_against_solution [%ty: int -> int list]
        ~gen:3 ~sampler:(fun x -> abs(sample_int())) "mystery" [0; 1; 10])
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

