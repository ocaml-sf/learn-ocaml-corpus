open Report
open Test_lib

let sample_int () =
  Random.int 90 + 10

let exercise_1 =
  Section ([ Text "Exercise 1: " ; Code "exchange" ],
           test_function_1_against_solution
             [%ty: int -> int] "exchange"
             [])

type solution = int * int

let sample_solution () =
  let grandpa = Random.int 39 + 61 in
  let grandson = Random.int (grandpa - 60) + 10 in
  (grandpa, grandson)

let exercise_2 =
  Section ([ Text "Exercise 2: " ; Code "is_valid_answer" ],
           let rec redact = function
             | Message (_, Informative)
               :: Message (ok, Success pts) :: rest ->
                 Message ([ Text "Computing" ; Code "[REDACTED FOR YOUR OWN GOOD]"], Informative)
                 :: Message (ok, Success pts) :: redact rest
             | msg :: rest -> msg :: redact rest
           | [] -> [] in
           test_function_1_against_solution
             [%ty: solution -> bool] "is_valid_answer"
             [ (72, 18) ] |> redact)

let exercise_3 =
  Section ([ Text "Exercise 3: " ; Code "find" ],
           test_function_1_against_solution
             [%ty: solution -> solution] "find"
             [ (99, 10) ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ]
