open Test_lib
open Report


let test_with_solution = 
  Section([Text "Tests"],
          test_function_2_against_solution
          [%ty: int -> int -> int*int*int] 
          "digits"
          ~sampler: (fun () -> (Random.int 999999999), (Random.int 10))
          ~gen:19
          [(0, 0)]
    )
    
let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [test_with_solution]