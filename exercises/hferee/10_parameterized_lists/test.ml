
open Test_lib
open Report

let l3 ty s =
  test_variable_property ty s
    (fun l ->
       [Message ([Code s],
                  if List.length l = 3
                  then Success 1
                  else Failure)])


let exercise =
  [
    Section([Text "Question 1"],
      l3 [%ty: int list] "l3_int" @
      l3 [%ty: float list] "l3_float" @
      l3 [%ty: bool list] "l3_bool" @
      l3 [%ty: (int*int) list] "l3_int_int");

    Section(
      [Code "length"],
       test_function_1_against_solution [%ty: int list -> int] "length" ~gen:0
         [[]; [1; 3; 5]] @
       test_function_1_against_solution [%ty: unit list -> int] "length" ~gen:0
         [[()]]);
    Section(
      [Code "map"],
       test_function_2_against_solution [%ty: (int -> string) -> int list ->
         string list] "map" ~gen:0
         [string_of_int, [1; 3; 5]] @
       test_function_2_against_solution
         [%ty: (string -> int) -> string list -> int list]
         "map" ~gen:0 [int_of_string, []]);
    Section(
      [Code "fold_left"],
       test_function_3_against_solution [%ty: (string -> int -> string) ->
         string -> int list -> string] "fold_left" ~gen:0
         [(fun s i -> s ^ string_of_int i), "", [1; 3; 5]] @
       test_function_3_against_solution
         [%ty: (float -> unit -> float) -> float -> unit list -> float]
         "fold_left" ~gen:0 [(fun x y -> x), 3.14, []]);
    Section(
      [Code "fold_right"],
       test_function_3_against_solution [%ty: (int -> string -> string) ->
         int list -> string -> string] "fold_right" ~gen:0
         [(fun i s -> s ^ string_of_int i), [1; 3; 5], ""] @
       test_function_3_against_solution [%ty: (unit -> float -> float)
           -> unit list -> float -> float] "fold_right" ~gen:0
         [(fun x y -> y), [], 3.14]);
]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

