open Test_lib
open Report

let correct_answer name =
 Section ([ Text "Exercise 1: " ; Code "solution" ],
  [Message ([ Text "Checking that " ; Code name ; Text "is correct "], Informative) ;
    Message ([ Text "Correct answer" ],  Success 5)])

let wrong_answer name =
 Section ([ Text "Exercise 1: " ; Code "solution" ],
  [Message ([ Text "Checking that " ; Code name ; Text "is correct "], Informative) ;
    Message ([ Text "Wrong answer" ],  Failure)])

let compatible_type ~expected:exp got =
  match Introspection.compatible_type exp ("Code." ^ got) with
  | Introspection.Absent     -> false
  | Introspection.Incompatible _ -> false
  | Introspection.Present () -> true

type correct = int   -> int   -> int   -> int
type incorrect1 = float -> int   -> int   -> int
type incorrect2 = int   -> float -> int   -> int
type incorrect3 = int   -> int   -> float -> int
type incorrect4 = int   -> int   -> int   -> float


let ex1 = 
  let a1 = compatible_type "correct" "q1" in
  let a2 = compatible_type "incorrect1" "q1" in
  let a3 = compatible_type "incorrect2" "q1" in
  let a4 = compatible_type "incorrect3" "q1" in
  let a5 = compatible_type "incorrect4" "q1" in

  match a1,a2,a3,a4,a5 with
    | true, false, false, false, false -> correct_answer "q1"
    | _  -> wrong_answer "q1"

let ex2 =
  set_progress "Correcting question 2" ;
  Section ([ Text "Exercise 2: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: int ] 
             "q2")

let ex3 =
  set_progress "Correcting question 3" ;
  Section ([ Text "Exercise 3: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "q3")
            
let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ ex1; ex2; ex3]
