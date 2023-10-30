open Test_lib
open Report
open List
open Random

let hhqR =
  set_progress "Grading exercise";
  Section
    ( [ Text "Testing function"; Code "hhq" ],
      test_function_3_against_solution [%ty: int -> int -> int -> int] "hhq"
        ~sampler:(fun () ->
          let () = Random.self_init () in
          (Random.int 3 + 1, Random.int 7 + 1, Random.int 14 + 1))
        ~gen:10
        [
          (2, 5, 10);
          (1, 4, 12);
          (2, 2, 20);
          (1, 4, 15);
          (2, 4, 17);
          (0, 1, 10);
          (5, 2, 10);
        ] )

let () = set_result @@ ast_sanity_check code_ast @@ fun () -> [ hhqR ]
