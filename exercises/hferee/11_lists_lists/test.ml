
open Test_lib
open Report

let exercise =
  [
    Section(
      [Code "list_list_get"],
      test_function_3_against_solution
        [%ty : int list list -> int -> int -> int option]
        ~gen: 0 "list_list_get" [[], 0, 0; [[1]], 0, 1; [[1]], 1, 0] @
      test_function_3_against_solution
        [%ty : unit list list -> int -> int -> unit option]
        ~gen: 0 "list_list_get" [[[]], 1, 0; [[()]], -1, 0]
    );

    Section(
      [Code "transpose"],
      test_function_1_against_solution [%ty : int list list -> int list list]
      ~gen: 0 "transpose" [[]; [[]]; [[1; 2; 3]; [4]; [5; 6]; []; [7; 8; 9; 10]]]
    );

    Section(
      [Code "merge"],
      test_function_2_against_solution [%ty : int list -> int list -> int list]
      ~gen: 0 "merge" [[], [1; 2; 3]; [1; 2; 3], []; [1], [0; 2; 3];
                       [1],[2; 3]; [1; 2; 5; 8; 11], [0; 6; 7; 8; 9; 12]] @
      test_function_1_against_solution [%ty : int list -> int list list]
      ~gen: 0 "wrap" [[]; [3]; [1; 3;5; 7; 9]] @
      test_function_1_against_solution [%ty : int list list -> int list list]
      ~gen: 0 "flatten_merge" [[]; [[]]; [[3]]; [[3; 5]; [2; 4; 7]];
                              [[1; 3; 5]; [2; 4; 8]; [2]; [1; 6]]] @
      test_function_1_against_solution [%ty : int list -> int list]
      ~gen: 5 "merge_sort" [[]; [3]; [1; 7; -1; 4; -3; 12]]
    )
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

