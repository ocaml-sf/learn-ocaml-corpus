open Test_lib
open Report
open List
open Random

let maioriaT = Section(
  [Text "Testing majority function"],
  test_function_1_against_solution
  [%ty: int array -> int]
  "majority"
  ~sampler: (sample_array ~min_size: 32 ~max_size: 32 ~dups: true (fun () -> Random.int(3) + 1))
  ~gen: 5
  [[|1; 1; 2; 1; 2; 3; 3; 2; 2; 2; 1; 2; 2; 3; 2; 2; 1; 1; 2; 1; 2; 3; 3; 2; 2; 2; 1; 2; 2; 3; 2; 2|];
  [|1; 3; 3; 3; 1; 2; 2; 3; 3; 3; 1; 1; 3; 3; 1; 2; 1; 3; 3; 3; 1; 2; 2; 3; 3; 3; 1; 1; 3; 3; 1; 2|];
  [|1; 2; 2; 1; 1; 1; 1; 3; 3; 1; 2; 1; 1; 3; 1; 3; 1; 2; 2; 1; 1; 1; 1; 3; 3; 1; 2; 1; 1; 3; 1; 3|];
  [|3; 1; 2; 1; 2; 3; 3; 3; 2; 2; 1; 3; 2; 1; 3; 3; 3; 1; 2; 1; 2; 3; 3; 3; 2; 2; 1; 3; 2; 1; 3; 3|];
  [|1; 1; 2; 1; 2; 3; 3; 2; 2; 2; 1; 2; 2; 3; 2; 2; 1; 1; 2; 1; 2; 3; 3; 2; 3; 3; 3; 3; 3; 3; 3; 1|]])

let mjrtyT = Section(
  [Text "Testing mjrty function"],
  test_function_1_against_solution
  [%ty: int list -> int]
  "mjrty"
  ~sampler: (sample_list ~min_size: 32 ~max_size: 32 ~dups: true (fun () -> Random.int(3) + 1))
  ~gen: 10
  [[1; 1; 2; 1; 2; 3; 3; 2; 2; 2; 1; 2; 2; 3; 2; 2; 1; 1; 2; 1; 2; 3; 3; 2; 2; 2; 1; 2; 2; 3; 2; 2];
  [1; 3; 3; 3; 1; 2; 2; 3; 3; 3; 1; 1; 3; 3; 1; 2; 1; 3; 3; 3; 1; 2; 2; 3; 3; 3; 1; 1; 3; 3; 1; 2];
  [1; 2; 2; 1; 1; 1; 1; 3; 3; 1; 2; 1; 1; 3; 1; 3; 1; 2; 2; 1; 1; 1; 1; 3; 3; 1; 2; 1; 1; 3; 1; 3];
  [3; 1; 2; 1; 2; 3; 3; 3; 2; 2; 1; 3; 2; 1; 3; 3; 3; 1; 2; 1; 2; 3; 3; 3; 2; 2; 1; 3; 2; 1; 3; 3];
  [1; 1; 2; 1; 2; 3; 3; 2; 2; 2; 1; 2; 2; 3; 2; 2; 1; 1; 2; 1; 2; 3; 3; 2; 3; 3; 3; 3; 3; 3; 3; 1]])

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () -> 
  [maioriaT;mjrtyT]