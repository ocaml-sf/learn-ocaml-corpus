open Test_lib
open Report

let sample_verify_right () = 
  ([], [])
let sample_verify_wrong () =
  ([], [])

let sample_verify () = 
  let () = Random.self_init () in
  if (Random.int 2) = 0
  then sample_verify_right ()
  else sample_verify_wrong ()

let verifyS () =
  test_function_2_against_solution
  [%ty: char list -> char list -> bool ]
  "verify"
  ~sampler: sample_verify
  ~gen: 0
  [(['a';'(';'a';'b';'(';'b';')';'c';'(';'o';'k';'a';')';'n';')';'h'], []); (['a';'(';'a';'b';'(';'b';')';'c';'(';'o';'k';'a';'n';')';'h'], [])]

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  verifyS ()
