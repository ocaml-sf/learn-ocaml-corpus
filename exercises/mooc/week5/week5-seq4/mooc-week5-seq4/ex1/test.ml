open Report
open Test_lib

let sample_array s = sample_array ~min_size: 1 s

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "rotate" ],
           let { before_reference ; before_user ; test } =
             array_arg_mutation_test_callbacks [%ty: int array] in
           test_function_1_against_solution ~gen: 5
             ~before_reference ~before_user ~test
             [%ty: int array -> unit] "rotate"
             [] @
           let { before_reference ; before_user ; test } =
             array_arg_mutation_test_callbacks [%ty: char array] in
           test_function_1_against_solution ~gen: 5
             ~before_reference ~before_user ~test
             [%ty: char array -> unit] "rotate"
             [ [||] ])

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "rotate_by" ],
           let { before_reference ; before_user ; test } =
             array_arg_mutation_test_callbacks [%ty: int array] in
           let before_reference a _ = before_reference a in
           let before_user a _ = before_user a in
           test_function_2_against_solution ~gen: 5
             ~before_reference ~before_user ~test
             [%ty: int array -> int -> unit] "rotate_by"
             [] @
           let { before_reference ; before_user ; test } =
             array_arg_mutation_test_callbacks [%ty: char array] in
           let before_reference a _ = before_reference a in
           let before_user a _ = before_user a in
           test_function_2_against_solution ~gen: 5
             ~before_reference ~before_user ~test
             [%ty: char array -> int -> unit] "rotate_by"
             [ [||], 0 ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ]
