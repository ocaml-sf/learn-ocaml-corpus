open Test_lib
open Report

let test_fractals =
  set_progress "Grading exercise";
  Section
    ( [ Text "Testing function"; Code "fractals" ],
      test_function_1_against_solution [%ty: int -> unit] "fractals"
        ~test_stdout:io_test_equals
        ~sampler:(fun () ->
          let () = Random.self_init () in
          int_of_float (2. ** float_of_int (Random.int 7)))
        ~gen:5
        [ 8; 0; -1; -2; 3; 100; 101; 128 ] )

let () = set_result @@ ast_sanity_check code_ast @@ fun () -> [ test_fractals ]
