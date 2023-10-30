
open Test_lib
open Report
let test_listes = [
  Nothing;
  OneMore(0, Nothing);
  OneMore(0, OneMore(1, Nothing));
  OneMore(0, OneMore(1, OneMore(2, OneMore(3, Nothing))));
  OneMore(0, OneMore(1, OneMore(0, OneMore(1, Nothing))))
  ]

let product l1 l2 =
    List.fold_left
      (fun x a -> List.fold_left
                    (fun y b -> (a, b) :: y)
                    x
                    l2)
      []
      l1


let test_listes2 = product test_listes test_listes

let test_nonempty_listes = List.tl test_listes

let exercise =
  [
    Section ( [ Code "hd" ],
              test_function_1_against_solution [%ty : liste -> int] ~gen:0 "hd"
                test_nonempty_listes);
    Section ( [ Code "tl" ],
              test_function_1_against_solution [%ty : liste -> liste] ~gen:0 "tl"
                test_listes);
    Section ( [ Code "length" ],
              test_function_1_against_solution [%ty : liste -> int] ~gen:0 "length"
                test_listes);
    Section ( [ Code "sum_list" ],
              test_function_1_against_solution [%ty : liste -> int] ~gen:0 "sum_list" test_listes);
    Section ( [ Code "concat" ],
              test_function_2_against_solution [%ty : liste -> liste -> liste] ~gen:0 "concat"
                test_listes2);
    Section ( [ Code "rev" ],
              test_function_1_against_solution [%ty : liste -> liste] ~gen:0 "rev" test_listes);
    Section ( [ Code "mem" ],
              test_function_2_against_solution [%ty : int -> liste -> bool] ~gen:0 "mem"
                (product [0; 1; 42] test_listes));
    Section ( [ Code "find_first" ],
              test_function_2_against_solution [%ty : int -> liste -> int] ~gen:0 "find_first"
                (product [0; 1; 42] test_listes));
    Section ( [ Code "find_last" ],
              test_function_2_against_solution [%ty : int -> liste -> int] ~gen:0 "find_last"
                (product [0; 1; 42] test_listes));
    Section ( [ Code "partition" ],
              test_function_2_against_solution [%ty : (int -> bool) -> liste -> liste * liste] ~gen:0 "partition"
              (product [fun x -> x mod 2 = 0] test_listes));
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () -> exercise

