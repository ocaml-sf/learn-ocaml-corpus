
open Test_lib
open Report

let notes = [[]; ["x", None]; [("a", Some 1.5); ("b", Some 15.5); ("c", None); ("d", Some 11.5)]]

let exercise =
  [
    Section(
      [Text "Question 1"],
      test_function_2_against_solution
        [%ty :(int -> float) -> int option -> float option] ~gen:0 "option_map"
        [float_of_int, None] @
      test_function_2_against_solution
        [%ty :(float -> int) -> float option -> int option] ~gen:0 "option_map"
        [int_of_float, Some 3.14]);

    Section(
      [Text "Question 2"],
      test_function_1_against_solution
        [%ty :(int list) -> int option] ~gen:0 "hd_opt"
        [[]; [4; 2]] @
      test_function_1_against_solution
        [%ty :(unit list) -> unit option] ~gen:0 "hd_opt"
        [[()]] @
      test_function_1_against_solution
        [%ty :(int list) -> int list option] ~gen:0 "tl_opt"
        [[]; [4;2]] @
      test_function_1_against_solution
        [%ty :(unit list) -> unit list option] ~gen:0 "tl_opt"
        [[()]]);

    Section(
      [Text "Question 3"],
      test_function_1_against_solution
        [%ty :(int list) -> int option] ~gen:0 "list_max"
        [[]; [4;2]] @
      test_function_1_against_solution
        [%ty :(float list) -> float option] ~gen:0 "list_max"
        [[3.4; 2.4]] @
      test_function_2_against_solution
        [%ty : (int -> int -> bool) -> (int list) -> int option] ~gen:0 "list_greatest"
        [(<), []; (>), [4;2]] @
      test_function_2_against_solution
        [%ty :(float -> float -> bool) -> (float list) -> float option] ~gen:0 "list_greatest"
        [(fun x y -> 1./.x < 1./.y), [3.4; 2.4; 0.001]]);

    Section(
      [Text "Question 4"],
      test_function_2_against_solution
        [%ty :(float -> bool) -> (float list) -> float option] ~gen:0 "find_opt"
        [(<) 2., [3.4; 2.4; 0.001]]);

    Section(
      [Text "Question 5"],
      test_function_2_against_solution
        [%ty :(float -> int option) -> (float list) -> int list]
        ~gen:0 "filter_map"
        [(fun x -> let i = int_of_float x in if float_of_int i = x then Some i
           else None), [3.4; 2.; 0.001]]);

    Section(
    [Text "Question 6"],
      test_function_2_against_solution
        [%ty : float -> ((float * int) list) -> int option]
        ~gen:0 "assoc_opt"
        [2.4, [(3.4, 1); (2.4, 2); (0.001, 3)]; 3.14, []]);


    Section(
      [Text "Question 7"],
      test_function_1_against_solution
        [%ty :(string * float option) list -> float option] ~gen:0 "average"
        notes @
      test_function_1_against_solution
        [%ty :(string * float option) list -> float option] ~gen:0 "average_present"
        notes @
      test_function_1_against_solution
        [%ty :(string * float option) list -> float option] ~gen:0 "max_grade"
        notes @
      test_function_1_against_solution
        [%ty :(string * float option) list -> string option] ~gen:0 "best"
        notes)
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

