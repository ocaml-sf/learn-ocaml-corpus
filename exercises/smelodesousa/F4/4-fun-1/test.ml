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


type ('a, 'b, 'c) correct_q1 = int * float
type ('a, 'b, 'c) incorrect1_q1  = bool -> float
type ('a, 'b, 'c) incorrect2_q1  = int -> bool

let ex1 = 
  let a1 = compatible_type "correct_q1" "q1" in
  let a2 = compatible_type "incorrect1_q1" "q1" in
  let a3 = compatible_type "incorrect2_q1" "q1" in

  match a1,a2,a3 with
    | true, false, false -> correct_answer "Exercise 1: " "q1"
    | _  -> wrong_answer "Exercise 1: " "q1"

(* 2 *)
type ('a, 'b, 'c) correct1_q2 = int -> (int -> 'a) -> int -> 'a
type ('a, 'b, 'c) correct2_q2 = int -> (int -> int) -> int -> int
type ('a, 'b, 'c) correct3_q2 = int -> (int -> float) -> int -> float
type ('a, 'b, 'c) incorrect1_q2  = float -> (int -> 'a) -> int -> 'a
type ('a, 'b, 'c) incorrect2_q2  = int -> (float -> 'a) -> int -> 'a
type ('a, 'b, 'c) incorrect3_q2  = int -> (int -> 'a) -> float -> 'a

let ex2 = 
  let a1 = compatible_type "correct1_q2" "q2" in
  let a2 = compatible_type "correct2_q2" "q2" in
  let a3 = compatible_type "correct3_q2" "q2" in
  let a4 = compatible_type "incorrect1_q2" "q2" in
  let a5 = compatible_type "incorrect2_q2" "q2" in
  let a6 = compatible_type "incorrect3_q2" "q2" in

  match a1,a2,a3,a4,a5,a6 with
    | true, true, true, false, false, false -> correct_answer "Exercise 2: " "q2"
    | _  -> wrong_answer "Exercise 2: " "q2"

(* 3 *)
type ('a, 'b, 'c) correct1_q3 = 'a -> ('b -> 'c) -> 'b -> 'c
type ('a, 'b, 'c) correct2_q3 = int -> (float -> bool) -> float -> bool
type ('a, 'b, 'c) correct3_q3 = float -> (string -> int) -> string -> int

let ex3 =
  let a1 = compatible_type "correct1_q3" "q3" in
  let a2 = compatible_type "correct2_q3" "q3" in
  let a3 = compatible_type "correct3_q3" "q3" in

  match a1,a2,a3 with
    | true, true, true -> correct_answer "Exercise 3: " "q3"
    | _  -> wrong_answer "Exercise 3: " "q3"

(* 4 *)
type ('a, 'b, 'c) correct1_q4 = (int -> 'a) -> (int -> int) -> int -> 'a
type ('a, 'b, 'c) correct2_q4 = (int -> string) -> (int -> int) -> int -> string
type ('a, 'b, 'c) correct3_q4 = (int -> bool) -> (int -> int) -> int -> bool
type ('a, 'b, 'c) incorrect1_q4 = (float -> 'a) -> (int -> int) -> int -> 'a
type ('a, 'b, 'c) incorrect2_q4 = (int -> 'a) -> (float -> int) -> int -> 'a
type ('a, 'b, 'c) incorrect3_q4 = (int -> 'a) -> (int -> float) -> int -> 'a
type ('a, 'b, 'c) incorrect4_q4 = (int -> 'a) -> (int -> int) -> float -> 'a


let ex4 =
  let long_name, short_name = "Exercise 4: ", "q4" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("correct2_"^short_name) short_name in
  let a3 = compatible_type ("correct3_"^short_name) short_name in
  let a4 = compatible_type ("incorrect1_"^short_name) short_name in
  let a5 = compatible_type ("incorrect2_"^short_name) short_name in
  let a6 = compatible_type ("incorrect3_"^short_name) short_name in
  let a7 = compatible_type ("incorrect4_"^short_name) short_name in

  match a1,a2,a3,a4,a5,a6,a7 with
    | true,true,true,false,false,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

(* 5 *)
type ('a, 'b, 'c) correct1_q5 = ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
type ('a, 'b, 'c) correct2_q5 = (int -> float) -> (bool -> int) -> bool -> float
type ('a, 'b, 'c) correct3_q5 = (string -> bool) -> (char -> string) -> char -> bool

let ex5 =
  let long_name, short_name = "Exercise 5: ", "q5" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("correct2_"^short_name) short_name in
  let a3 = compatible_type ("correct3_"^short_name) short_name in

  match a1,a2,a3 with
    | true,true,true -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

(* 6 *)
type ('a, 'b, 'c) correct1_q6 = 'a -> ('a -> 'b) -> ('b -> 'c) -> 'c
type ('a, 'b, 'c) correct2_q6 = int -> (int -> bool) -> (bool -> char) -> char
type ('a, 'b, 'c) correct3_q6 = float -> (float -> string) -> (string -> int) -> int

let ex6 =
  let long_name, short_name = "Exercise 6: ", "q6" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("correct2_"^short_name) short_name in
  let a3 = compatible_type ("correct3_"^short_name) short_name in

  match a1,a2,a3 with
    | true,true,true -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name


(* 7 *)
type ('a, 'b, 'c) correct1_q7 = int list ref
type ('a, 'b, 'c) incorrect1_q7 = float list ref
type ('a, 'b, 'c) incorrect2_q7 = string ref

let ex7 =
  let long_name, short_name = "Exercise 7: ", "q7" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("incorrect1_"^short_name) short_name in
  let a3 = compatible_type ("incorrect2_"^short_name) short_name in

  match a1,a2,a3 with
    | true,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

(* 8 *)
type ('a, 'b, 'c) correct1_q8 = _weak1 list ref
type ('a, 'b, 'c) incorrect1_q8 = float list ref
type ('a, 'b, 'c) incorrect2_q8 = string ref

let ex8 =
  let long_name, short_name = "Exercise 8: ", "q8" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("incorrect1_"^short_name) short_name in
  let a3 = compatible_type ("incorrect2_"^short_name) short_name in

  match a1,a2,a3 with
    | true,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

(* 9 *)
type ('a, 'b, 'c) correct1_q9 = (int -> int) list
type ('a, 'b, 'c) incorrect1_q9 = (float -> int) list
type ('a, 'b, 'c) incorrect2_q9 = (int -> float) list
type ('a, 'b, 'c) incorrect3_q9 = float

let ex9 =
  let long_name, short_name = "Exercise 9: ", "q9" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("incorrect1_"^short_name) short_name in
  let a3 = compatible_type ("incorrect2_"^short_name) short_name in
  let a4 = compatible_type ("incorrect3_"^short_name) short_name in

  match a1,a2,a3,a4 with
    | true,false,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

(* 10 *)
type ('a, 'b, 'c) correct1_q10 = int
type ('a, 'b, 'c) incorrect1_q10 = float

let ex10 =
  let long_name, short_name = "Exercise 10: ", "q10" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("incorrect1_"^short_name) short_name in

  match a1,a2 with
    | true,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

(* 11 *)
type ('a, 'b, 'c) correct1_q11 = ('a -> 'b) -> ('b list -> 'c) -> 'a list -> 'c
type ('a, 'b, 'c) correct2_q11 = (int -> float) -> (float list -> string) -> int list -> string
type ('a, 'b, 'c) correct3_q11 = (bool -> char) -> (char list -> int) -> bool list -> int
type ('a, 'b, 'c) incorrect1_q11 = ('a -> 'b) -> (int -> 'c) -> 'a list -> 'c
type ('a, 'b, 'c) incorrect1_q11 = ('a -> 'b) -> ('b list -> 'c) -> float -> 'c


let ex11 =
  let long_name, short_name = "Exercise 11: ", "q11" in
  let a1 = compatible_type ("correct1_"^short_name) short_name in
  let a2 = compatible_type ("correct2_"^short_name) short_name in
  let a3 = compatible_type ("correct3_"^short_name) short_name in
  let a4 = compatible_type ("incorrect1_"^short_name) short_name in
  let a5 = compatible_type ("incorrect2_"^short_name) short_name in

  match a1,a2,a3,a4,a5 with
    | true,true,true,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name


let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ex1; ex2; ex3; ex4; ex5; ex6; ex7; ex8; ex9; ex10; ex11]
