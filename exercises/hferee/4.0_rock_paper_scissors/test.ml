open Test_lib
open Report

let testint name =
   Section ([ Code name ], test_variable_against_solution [%ty : int] name)

let moves = [Rock; Paper; Scissors]
let move_pairs = List.flatten(List.map (fun y -> List.map (fun x -> x, y) moves) moves)

let sample_o () = match Random.int 3 with
  | 0 -> Victory
  | 1 -> Defeat
  | 2 -> Draw

let sample_s () =
  { player = abs (sample_int()); opponent = abs (sample_int());}, sample_o()

let exercise =
  [
    Section ([Code "round"],
             test_function_2_against_solution [%ty : move -> move -> outcome]
                                              ~gen:0
                                              "round" move_pairs);

    Section ([Code "score"],
             test_function_2_against_solution [%ty : scoreboard -> outcome ->
               scoreboard]
               ~gen: 20 ~sampler:sample_s "score" []);]
let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

