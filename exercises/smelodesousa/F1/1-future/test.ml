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

type correct = int   -> string -> string
type incorrect1 = float -> string -> string
type incorrect2 = int   -> float  -> string
type incorrect3 = int   -> string -> float

let ex1 = 
  let r1 = compatible_type "correct" "p1" in
  let r2 = compatible_type "incorrect1" "p1" in
  let r3 = compatible_type "incorrect2" "p1" in
  let r4 = compatible_type "incorrect3" "p1" in
  match r1,r2,r3,r4 with
    | true, false, false, false -> correct_answer "p1"
    | _  -> wrong_answer "p1"

let ex2 =
  set_progress "Grading exercise 2" ;
  Section ([ Text "Exercise 2: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: string ] 
             "p2")

let ex3 =
  set_progress "Grading exercise 3" ;
  Section ([ Text "Exercise 3: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "p3")
            
let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ ex1; ex2; ex3 ]
