open Report
open Test_lib

let sample_int () =
  Random.int 30 - 15

let sample_array sample =
  sample_array ~min_size: 2 ~max_size: 12 ~dups: false ~sorted: false sample

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section ([ Text "Exercise 1: " ; Code "min" ],
             test_function_1_against_solution
               [%ty: int array -> int] "min"
               [ (* auto gen *) ]) ;
    Section ([ Text "Exercise 2: " ; Code "min_index" ],
             test_function_1_against_solution
               [%ty: int array -> int] "min_index"
               [ (* auto gen *) ]) ] @
  test_variable_property
    [%ty: string] "it_scales"
    (fun s -> match String.trim s with
       | "yes" | "YES" | "y" | "Y" | "Yes" ->
           [ Message ([ Text "I think you're wrong." ], Informative) ;
             Message ([ Text "Let's see in the next exercise" ], Important) ]
       | "no" | "NO" | "n" | "N" | "No"  ->
           [ Message ([ Text "You're probably right." ], Informative) ;
             Message ([ Text "Let's write bettter solution in the next exercise" ], Important) ]
       | _ ->
           [ Message ([ Text "I did not understand your answer." ], Failure) ])
