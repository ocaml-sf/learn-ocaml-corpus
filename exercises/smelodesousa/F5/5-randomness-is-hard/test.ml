open Test_lib
open Report

let random_state = ref (Random.get_state ())

let test_with_solution =
  set_progress "Grading exercise";
  Section
    ( [ Text "Testing function"; Code "knuth_shuffle" ],
      test_function_1_against_solution [%ty: int array -> int array]
        "knuth_shuffle"
        ~before_reference:(fun _ -> random_state := Random.get_state ())
        ~before_user:(fun _ -> Random.set_state !random_state)
        ~gen:17
        [ [||]; [| 1 |]; [| -5; 100; 125 |] ] )

let () =
  set_result @@ ast_sanity_check code_ast @@ fun () -> [ test_with_solution ]
