open Report
open Test_lib

let sampler () = Random.int 10000

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section ([ Text "Exercise 1:" ; Code "multiple_of" ],
             test_function_2_against_solution
               [%ty: int -> int -> bool] "multiple_of"
               [ 4, 2 ; 8,3 ; 13, 2 ; 12, 4 ]) ;
    Section ([ Text "Exercise 2:" ; Code "integer_square_root" ],
             test_function_1_against_solution ~sampler
               [%ty: int -> int] "integer_square_root"
               [ 2 ; 16 ; 144 ; 89 ]) ]
