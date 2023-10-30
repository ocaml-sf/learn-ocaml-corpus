(*
   Euclid function: a, b contained in [0, +infinity[

   Test Cases:


   | Test Case |     a    |    b      |
   | :-------: | :------: | :-------: |
   | T1        |   0      |    0      |
   | T2        |   0      | non-zero  |
   | T3        | non-zero |    0      |
   | T4        | positive | positive  |
   | T5        | positive | negative  |
   | T6        | negative | positive  |
   | T7        | negative | negative  |

   | Test Case |   a   |   b   |
   | :-------: | :---: | :---: |
   | T1        |  0    |  0    |
   | T2        |  0    | -1    |
   | T3        | -1    |  0    |
   | T4        |  1    |  1    |
   | T5        |  1    | -1    |
   | T6        | -1    |  1    |
   | T7        | -1    | -1    |
*)

open Test_lib
open Report

let test_with_solution =
  Section
    ( [ Text "Tests" ],
      test_function_2_against_solution [%ty: int -> int -> int] "euclid"
        ~sampler:(fun () -> (Random.int 999999, Random.int 999999))
        ~gen:13
        [ (0, 0) ] )

let test_with_solution_both_negative =
  Section
    ( [ Text "Tests" ],
      test_function_2_against_solution [%ty: int -> int -> int] "euclid"
        ~sampler:(fun () ->
          (Random.int 999999 - 999999, Random.int 999999 - 999999))
        ~gen:2 [] )

let test_with_solution_a_negative =
  Section
    ( [ Text "Tests" ],
      test_function_2_against_solution [%ty: int -> int -> int] "euclid"
        ~sampler:(fun () -> (Random.int 999999 - 999999, Random.int 999999))
        ~gen:2 [] )

let test_with_solution_b_negative =
  Section
    ( [ Text "Tests" ],
      test_function_2_against_solution [%ty: int -> int -> int] "euclid"
        ~sampler:(fun () -> (Random.int 999999, Random.int 999999 - 999999))
        ~gen:2 [] )

let () =
  set_result @@ ast_sanity_check code_ast
  @@ fun () ->
  [
    test_with_solution;
    test_with_solution_both_negative;
    test_with_solution_a_negative;
    test_with_solution_b_negative;
  ]
