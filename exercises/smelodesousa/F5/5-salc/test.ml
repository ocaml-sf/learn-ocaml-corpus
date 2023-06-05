open Test_lib
open Report
open List
open Random

let sort1T =
  set_progress "Grading exercise 1";
  Section
    ( [ Text "Exercise 1: "; Code "sort1" ],
      test_function_1_against_solution [%ty: int array -> int array] "sort1"
        ~sampler:
          (sample_array ~min_size:15 ~max_size:45 ~dups:true (fun () ->
               let () = Random.self_init () in
               Random.int 100))
        ~gen:10 [] )

let sort2T =
  set_progress "Grading exercise 2";
  Section
    ( [ Text "Exercise 2: "; Code "sort2" ],
      test_function_1_against_solution [%ty: int list -> int list] "sort2"
        ~sampler:
          (sample_list ~min_size:15 ~max_size:45 ~dups:true (fun () ->
               let () = Random.self_init () in
               Random.int 100))
        ~gen:10 [] )

let sort3T =
  set_progress "Grading exercise 3";
  Section
    ( [ Text "Exercise 3: "; Code "sort3" ],
      test_function_1_against_solution [%ty: int array -> int array] "sort3"
        ~sampler:
          (sample_array ~min_size:15 ~max_size:45 ~dups:true (fun () ->
               let () = Random.self_init () in
               Random.int 100))
        ~gen:10 [] )

let sort4T =
  set_progress "Grading exercise 4";
  Section
    ( [ Text "Exercise 4: "; Code "sort4" ],
      test_function_1_against_solution [%ty: int list -> int list] "sort4"
        ~sampler:
          (sample_list ~min_size:15 ~max_size:45 ~dups:true (fun () ->
               let () = Random.self_init () in
               Random.int 200))
        ~gen:10
        [ [ 121; 17; 191; 32; 19; 91 ] ] )

let () =
  set_result @@ ast_sanity_check code_ast
  @@ fun () -> [ sort1T; sort2T; sort3T; sort4T ]
