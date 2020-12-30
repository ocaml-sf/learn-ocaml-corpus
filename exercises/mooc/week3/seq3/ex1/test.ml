open Report
open Test_lib

let sample_action () =
  match Random.int 3 with
  | 0 -> Eat
  | 1 -> GoToRestaurant
  | _ -> Sleep

let sample_state () =
  match Random.int 3 with
  | 0 -> Happy
  | 1 -> Hungry
  | _ -> Tired

let sample_location () =
  match Random.bool () with
  | true -> Appartment
  | false -> Restaurant

let sample_character =
  let names =
    [| "Yann" ; "Roberto" ; "Ralf" ; "Benjamin" ; "Cagdas" ; "Gregoire" |] in
  fun () ->
    { name = names.(Random.int (Array.length names)) ;
      state = sample_state () ;
      location = sample_location () }

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "apply_action" ],
           test_function_2_against_solution
             [%ty: character -> action -> character] "apply_action"
             [])

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "possible_changes_for_character" ],
           test_function_1_against_solution
             [%ty: character -> state list] "possible_changes_for_character"
             [])

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "describe_state" ],
           test_function_1_against_solution
             [%ty: state -> string] "describe_state"
             [ Tired ])

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "tell_action" ],
           test_function_1_against_solution
             [%ty: action -> string] "tell_action"
             [ Sleep ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ]
