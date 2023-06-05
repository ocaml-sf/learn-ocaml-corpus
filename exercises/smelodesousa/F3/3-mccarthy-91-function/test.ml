open Test_lib
open Report

let test_a_with_solution = 
  Section([Text "Testing"; Code("mccarthy")],
          test_function_1_against_solution
          [%ty: int -> int] 
          "mccarthy"
          ~sampler: (fun () -> (Random.int 200))
          ~gen:19
          [200]
    )

let test_b_with_solution = 
  Section([Text "Testing"; Code("f91")],
          test_function_1_against_solution
          [%ty: int -> int]
          "f91"
          ~sampler: (fun () -> (Random.int 200))
          ~gen:17
          [99; 100; 101]
    )

let test_c_with_solution = 
  Section([Text "Testing"; Code("mccarthy_is_f91")],
          test_function_2_against_solution
          [%ty: int -> int -> bool]
          "mccarthy_is_f91"
          ~sampler: (fun () -> let i = Random.int 100 in (i, (Random.int 100) + i))
          ~gen:17
          [(70, 120)]
    )

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [test_a_with_solution; test_b_with_solution; test_c_with_solution]