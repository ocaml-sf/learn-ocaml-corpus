open Test_lib
open Report

let sample_value () =
  let () = Random.self_init () in
  Random.int 201 - 100

let sample_score () =
  let l = sample_list ~min_size:25 ~max_size:75 sample_value () in
  (List.length l, l)

let scoreS =
  Section
    ( [ Text "Testing function score" ],
      test_function_2_against_solution [%ty: int -> int list -> int] "score"
        ~sampler:sample_score ~gen:9
        [
          (1, [ -3 ]);
          (4, [ 2; -2; 3; -1 ]);
          (16, [ -1; 1; 3; -8; 3; -2; 5; 10; -2; -5; 4; 1; -7; 13; -8; 4 ]);
          (4, [ -1; -2; -3; -4 ]);
        ] )

let () = set_result @@ ast_sanity_check code_ast @@ fun () -> [ scoreS ]
