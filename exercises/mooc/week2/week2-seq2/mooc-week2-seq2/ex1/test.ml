open Report
open Test_lib

let sample_date () =
  { year = Random.int 10 - 2 ;
    month = Random.int 7 ;
    day = Random.int 6 ;
    hour = Random.int 5 - 1 ;
    minute = Random.int 4 - 1 }

let exercise_1 =
  Section ([ Text "Exercise 1:" ; Code "wellformed" ],
           test_function_1_against_solution
             [%ty: date -> bool] "wellformed"
             [ { year = 1; month = 1; day = 1; hour = 0; minute = 0 } ;
               { year = 1000; month = 1; day = 1; hour = 1; minute = 0 } ;
               { year = 1; month = 1; day = 1; hour = 1; minute = -1 } ;
               { year = -1; month = 1; day = 1; hour = 1; minute = 1 } ])

let sample_date () =
  { year = Random.int 10 + 1 ;
    month = Random.int 5 + 1 ;
    day = Random.int 4 + 1 ;
    hour = Random.int 3 ;
    minute = Random.int 2 }

let sample_int () = Random.int 200

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ;
    Section ([ Text "Exercise 2:" ; Code "next" ],
             test_function_1_against_solution
               [%ty: date -> date] "next"
               [ (* auto gen *) ]) ;
    Section ([ Text "Exercise 3:" ; Code "of_int" ],
             test_function_1_against_solution
               [%ty: int -> date] "of_int"
               [ (* auto gen *) ]) ]
