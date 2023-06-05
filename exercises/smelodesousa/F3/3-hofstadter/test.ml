open Test_lib
open Report

let factor_sampler () = 
  let () = Random.self_init () in
    ((Random.int 50) - 5)

let hfmS = Section (
  [Text "Testing hfm function"], 
  test_function_1_against_solution
    [%ty: int -> (int * int) ]
    "hfm"
    ~sampler: factor_sampler
    ~gen: 8
    [(-100); 0] )

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [hfmS]
