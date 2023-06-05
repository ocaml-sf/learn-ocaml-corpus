open Test_lib
open Report

let correct_answer id name =
 Section ([ Text id ; Code "solution" ],
  [Message ([ Text "Checking that " ; Code name ; Text "is correct "], Informative) ;
    Message ([ Text "Correct answer" ],  Success 5)])

let wrong_answer id name =
 Section ([ Text id ; Code "solution" ],
  [Message ([ Text "Checking that " ; Code name ; Text "is correct "], Informative) ;
    Message ([ Text "Wrong answer" ],  Failure)])

let compatible_type ~expected:exp got =
  match Introspection.compatible_type exp ("Code." ^ got) with
  | Introspection.Absent     -> false
  | Introspection.Incompatible _ -> false
  | Introspection.Present () -> true

(* 1 *)
type ('a, 'b, 'c) correct1_q1 = int list
type ('a, 'b, 'c) incorrect1_q1 = float list
type ('a, 'b, 'c) incorrect2_q1 = bool

let ex1 =
  let long_name, short_name = "Exercise 1: ", "q1" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("incorrect1_"^short_name) short_name in
  let a3 = compatible_type ("incorrect2_"^short_name) short_name in


  match a1,a2,a3 with
    | true,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

let ex2 =
  set_progress "Correcting question 2" ;
  Section ([ Text "Exercise 2: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "q2")

let ex3 =
  set_progress "Correcting question 3" ;
  Section ([ Text "Exercise 3: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "q3")


(* 4 *)
type ('a, 'b, 'c) correct1_q4 = ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
type ('a, 'b, 'c) correct2_q4 = (int -> bool -> char) -> (int -> bool) -> int -> char
type ('a, 'b, 'c) correct3_q4 = (float -> int -> string) -> (float -> int) -> float -> string

let ex4 =
  let long_name, short_name = "Exercise 4: ", "q4" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("correct2_"^short_name) short_name in
  let a3 = compatible_type ("correct3_"^short_name) short_name in

  match a1,a2,a3 with
    | true,true,true -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name


(* 5 *)
type ('a, 'b, 'c) correct1_q5 = int list
type ('a, 'b, 'c) incorrect1_q5 = float list
type ('a, 'b, 'c) incorrect2_q5 = char

let ex5 =
  let long_name, short_name = "Exercise 5: ", "q5" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("incorrect1_"^short_name) short_name in
  let a3 = compatible_type ("incorrect2_"^short_name) short_name in

  match a1,a2,a3 with
    | true,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

let ex6 =
  set_progress "Correcting question 6" ;
  Section ([ Text "Exercise 6: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "q6")


(* 7 *)
type ('a, 'b, 'c) correct1_q7 = _weak1 -> _weak2 -> _weak1
type ('a, 'b, 'c) incorrect1_q7 = int -> _weak2 -> _weak1
type ('a, 'b, 'c) incorrect2_q7 = _weak1 -> int -> _weak1
type ('a, 'b, 'c) incorrect3_q7 = _weak1 -> _weak2 -> int

let ex7 =
  let long_name, short_name = "Exercise 7: ", "q7" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("incorrect1_"^short_name) short_name in
  let a3 = compatible_type ("incorrect2_"^short_name) short_name in
  let a4 = compatible_type ("incorrect3_"^short_name) short_name in

  match a1,a2,a3,a4 with
    | true,false,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name


(* 8 *)
type ('a, 'b, 'c) correct1_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect1_q8 = ((((float -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect2_q8 = ((((int -> float) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect3_q8 = ((((int -> int) -> float -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect4_q8 = ((((int -> int) -> int -> float) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect5_q8 = ((((int -> int) -> int -> int) -> (float -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect6_q8 = ((((int -> int) -> int -> int) -> (int -> float) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect7_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> float -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect8_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> float) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect9_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((float -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect10_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> float) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect11_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> float -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect12_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> float) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect13_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (float -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect14_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> float) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect15_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> float -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect16_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> float) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect17_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((float -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect18_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> float) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect19_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> float -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect20_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> float) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect21_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (float -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect22_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> float) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect23_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> float -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect24_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> float) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect25_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((float -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect26_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> float) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect27_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> float -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect28_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> float) -> (int -> int) -> int -> int
type ('a, 'b, 'c) incorrect29_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (float -> int) -> int -> int
type ('a, 'b, 'c) incorrect30_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> float) -> int -> int
type ('a, 'b, 'c) incorrect31_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> float -> int
type ('a, 'b, 'c) incorrect32_q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> float

let ex8 =
  let long_name, short_name = "Exercise 8: ", "q8" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("incorrect1_"^short_name) short_name in
  let a3 = compatible_type ("incorrect2_"^short_name) short_name in
  let a4 = compatible_type ("incorrect3_"^short_name) short_name in
  let a5 = compatible_type ("incorrect4_"^short_name) short_name in
  let a6 = compatible_type ("incorrect5_"^short_name) short_name in
  let a7 = compatible_type ("incorrect6_"^short_name) short_name in
  let a8 = compatible_type ("incorrect7_"^short_name) short_name in
  let a9 = compatible_type ("incorrect8_"^short_name) short_name in
  let a10 = compatible_type ("incorrect9_"^short_name) short_name in
  let a11 = compatible_type ("incorrect10_"^short_name) short_name in
  let a12 = compatible_type ("incorrect11_"^short_name) short_name in
  let a13 = compatible_type ("incorrect12_"^short_name) short_name in
  let a14 = compatible_type ("incorrect13_"^short_name) short_name in
  let a15 = compatible_type ("incorrect14_"^short_name) short_name in
  let a16 = compatible_type ("incorrect15_"^short_name) short_name in
  let a17 = compatible_type ("incorrect16_"^short_name) short_name in
  let a18 = compatible_type ("incorrect17_"^short_name) short_name in
  let a19 = compatible_type ("incorrect18_"^short_name) short_name in
  let a20 = compatible_type ("incorrect19_"^short_name) short_name in
  let a21 = compatible_type ("incorrect20_"^short_name) short_name in
  let a22 = compatible_type ("incorrect21_"^short_name) short_name in
  let a23 = compatible_type ("incorrect22_"^short_name) short_name in
  let a24 = compatible_type ("incorrect23_"^short_name) short_name in
  let a25 = compatible_type ("incorrect24_"^short_name) short_name in
  let a26 = compatible_type ("incorrect25_"^short_name) short_name in
  let a27 = compatible_type ("incorrect26_"^short_name) short_name in
  let a28 = compatible_type ("incorrect27_"^short_name) short_name in
  let a29 = compatible_type ("incorrect28_"^short_name) short_name in
  let a30 = compatible_type ("incorrect29_"^short_name) short_name in
  let a31 = compatible_type ("incorrect30_"^short_name) short_name in
  let a32 = compatible_type ("incorrect31_"^short_name) short_name in
  let a33 = compatible_type ("incorrect32_"^short_name) short_name in

  match a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33 with
    | true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ ex1; ex2; ex3; ex4; ex5; ex6; ex7; ex8 ]
