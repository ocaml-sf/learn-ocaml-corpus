open Test_lib
open Report

let testSubbag () = 
  test_function_1_against_solution
    [%ty: char list -> char list list] 
    "subbag"
    ~sampler: (sample_list ~min_size: 2 ~max_size: 4 sample_char)
    ~gen: 10
    []

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  testSubbag ()
