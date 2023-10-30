open Test_lib
open Report
open List
open Random
open Solution

let mountains_sampler () = (Random.int 15, Random.int 15)

let test_mountains =
  set_progress "Grading exercise";
  Section
    ( [ Text "Testing function"; Code "mountains" ],
      test_function_2_against_solution [%ty: int -> int -> int option]
        "mountains"
        ~sampler:(fun () -> mountains_sampler ())
        ~gen:15
        [ (4, 3); (1, 0); (1, 1); (30, 30) ] )

let () = set_result @@ ast_sanity_check code_ast @@ fun () -> [ test_mountains ]
