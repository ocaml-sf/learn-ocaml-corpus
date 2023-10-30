open Test_lib
open Report

let halveS () =
  test_function_1_against_solution
  [%ty: int list -> (int list * int list)]
  "halve"
  ~sampler: (sample_list ~min_size: 10 ~max_size: 50 (fun () -> let () = Random.self_init () in  (Random.int 50)))
  ~gen: 10
  [([]); [1]]

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  halveS ()
