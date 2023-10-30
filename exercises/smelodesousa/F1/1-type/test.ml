open Test_lib
open Report

let correct_answer id name =
  Section
    ( [ Text id; Code "solution" ],
      [
        Message ([ Text "Grading exercise "; Code name ], Informative);
        Message ([ Text "Correct answer" ], Success 5);
      ] )

let wrong_answer id name =
  Section
    ( [ Text id; Code "solution" ],
      [
        Message ([ Text "Grading exercise "; Code name ], Informative);
        Message ([ Text "Wrong answer" ], Failure);
      ] )

let compatible_type ~expected:exp got =
  match Introspection.compatible_type exp ("Code." ^ got) with
  | Introspection.Absent -> false
  | Introspection.Incompatible _ -> false
  | Introspection.Present () -> true

(* 1 *)
type ('a, 'b) correct_q1 = int -> int
type ('a, 'b) incorrect1_q1 = float -> int
type ('a, 'b) incorrect2_q1 = int -> float

let ex1 =
  let a1 = compatible_type "correct_q1" "q1" in
  let a2 = compatible_type "incorrect1_q1" "q1" in
  let a3 = compatible_type "incorrect2_q1" "q1" in

  match (a1, a2, a3) with
  | true, false, false -> correct_answer "Exercise 1: " "q1"
  | _ -> wrong_answer "Exercise 1: " "q1"

(* 2 *)
type ('a, 'b) correct_q2 = float -> int -> float
type ('a, 'b) incorrect1_q2 = bool -> int -> float
type ('a, 'b) incorrect2_q2 = float -> bool -> float
type ('a, 'b) incorrect3_q2 = float -> int -> bool

let ex2 =
  let a1 = compatible_type "correct_q2" "q2" in
  let a2 = compatible_type "incorrect1_q2" "q2" in
  let a3 = compatible_type "incorrect2_q2" "q2" in
  let a4 = compatible_type "incorrect3_q2" "q2" in
  match (a1, a2, a3, a4) with
  | true, false, false, false -> correct_answer "Exercise 2: " "q2"
  | _ -> wrong_answer "Exercise 2: " "q2"

(* 3 *)
type ('a, 'b) correct_q3 = float
type ('a, 'b) incorrect_q3 = int

let ex3 =
  let a1 = compatible_type "correct_q3" "q3" in
  let a2 = compatible_type "incorrect_q3" "q3" in
  match (a1, a2) with
  | true, false -> correct_answer "Exercise 3: " "q3"
  | _ -> wrong_answer "Exercise 3: " "q3"

(* 4 *)
type ('a, 'b) correct_q4 = float
type ('a, 'b) incorrect_q4 = int

let ex4 =
  let a1 = compatible_type "correct_q4" "q4" in
  let a2 = compatible_type "incorrect_q4" "q4" in
  match (a1, a2) with
  | true, false -> correct_answer "Exercise 4: " "q4"
  | _ -> wrong_answer "Exercise 4: " "q4"

(* 5 *)
type ('a, 'b) correct_q5 = bool -> bool * bool -> bool
type ('a, 'b) incorrect1_q5 = int -> bool * bool -> bool
type ('a, 'b) incorrect2_q5 = bool -> int * bool -> bool
type ('a, 'b) incorrect3_q5 = bool -> bool * int -> bool
type ('a, 'b) incorrect4_q5 = bool -> bool * bool -> int

let ex5 =
  let a1 = compatible_type "correct_q5" "q5" in
  let a2 = compatible_type "incorrect1_q5" "q5" in
  let a3 = compatible_type "incorrect2_q5" "q5" in
  let a4 = compatible_type "incorrect3_q5" "q5" in
  let a5 = compatible_type "incorrect4_q5" "q5" in
  match (a1, a2, a3, a4, a5) with
  | true, false, false, false, false -> correct_answer "Exercise 5: " "q5"
  | _ -> wrong_answer "Exercise 5: " "q5"

(* 6 *)
type ('a, 'b) correct1_q6 = 'a -> 'a -> 'b -> bool * 'b
type ('a, 'b) correct2_q6 = int -> int -> float -> bool * float
type ('a, 'b) correct3_q6 = float -> float -> int -> bool * int
type ('a, 'b) incorrect_q6 = 'b -> 'b -> 'b -> int * 'b

let ex6 =
  let a1 = compatible_type "correct1_q6" "q6" in
  let a2 = compatible_type "correct2_q6" "q6" in
  let a3 = compatible_type "correct3_q6" "q6" in
  let a4 = compatible_type "incorrect_q6" "q6" in

  match (a1, a2, a3, a4) with
  | true, true, true, false -> correct_answer "Exercise 6: " "q6"
  | _ -> wrong_answer "Exercise 6: " "q6"

(* 7 *)
type ('a, 'b) correct1_q7 = 'a -> 'a -> 'a -> bool * 'a
type ('a, 'b) correct2_q7 = int -> int -> int -> bool * int
type ('a, 'b) correct3_q7 = float -> float -> float -> bool * float
type ('a, 'b) incorrect_q7 = 'b -> 'b -> 'b -> int * 'b

let ex7 =
  let a1 = compatible_type "correct1_q7" "q7" in
  let a2 = compatible_type "correct2_q7" "q7" in
  let a3 = compatible_type "correct3_q7" "q7" in
  let a4 = compatible_type "incorrect_q7" "q7" in

  match (a1, a2, a3, a4) with
  | true, true, true, false -> correct_answer "Exercise 7: " "q7"
  | _ -> wrong_answer "Exercise 7: " "q7"

(* 8 *)
type ('a, 'b) correct1_q8 = 'a -> 'a -> 'a -> bool * 'a
type ('a, 'b) correct2_q8 = int -> int -> int -> bool * int
type ('a, 'b) correct3_q8 = float -> float -> float -> bool * float
type ('a, 'b) incorrect_q8 = 'b -> 'b -> 'b -> int * 'b

let ex8 =
  let a1 = compatible_type "correct1_q8" "q8" in
  let a2 = compatible_type "correct2_q8" "q8" in
  let a3 = compatible_type "correct3_q8" "q8" in
  let a4 = compatible_type "incorrect_q8" "q8" in

  match (a1, a2, a3, a4) with
  | true, true, true, false -> correct_answer "Exercise 8: " "q8"
  | _ -> wrong_answer "Exercise 8: " "q8"

let () =
  set_result @@ ast_sanity_check code_ast
  @@ fun () -> [ ex1; ex2; ex3; ex4; ex5; ex6; ex7; ex8 ]
