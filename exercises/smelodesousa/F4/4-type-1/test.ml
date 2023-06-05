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
type 'a correct1_p1 = 'a ref -> 'a
type 'a correct2_p1 = int ref -> int
type 'a correct3_p1 = float ref -> float
type 'a wrong1_p1 = int -> 'a

let ex1 =
  let long_name, short_name = "Exercise 1 ", "p1" in
  let r1 = compatible_type ("correct1_"^short_name) short_name in
  let r2 = compatible_type ("correct2_"^short_name) short_name in
  let r3 = compatible_type ("correct3_"^short_name) short_name in
  let r4 = compatible_type ("wrong1_"^short_name) short_name in

  match r1,r2,r3,r4 with
    | true,true,true,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

(* 2 *)
type 'a correct1_p2 = ('a -> bool) -> 'a -> ('a -> 'a -> bool) -> 'a -> bool
type 'a correct2_p2 = (int -> bool) -> int -> (int -> int -> bool) -> int -> bool
type 'a correct3_p2 = (float -> bool) -> float -> (float -> float -> bool) -> float -> bool
type 'a wrong1_p2 = ('a -> int) -> 'a -> ('a -> 'a -> bool) -> 'a -> bool
type 'a wrong2_p2 = ('a -> bool) -> 'a -> ('a -> 'a -> int) -> 'a -> bool
type 'a wrong3_p2 = ('a -> bool) -> 'a -> ('a -> 'a -> bool) -> 'a -> int

let ex2 =
  let long_name, short_name = "Exercise 2 ", "p2" in
  let r1 = compatible_type ("correct1_"^short_name) short_name in
  let r2 = compatible_type ("correct2_"^short_name) short_name in
  let r3 = compatible_type ("correct3_"^short_name) short_name in
  let r4 = compatible_type ("wrong1_"^short_name) short_name in
  let r5 = compatible_type ("wrong2_"^short_name) short_name in
  let r6 = compatible_type ("wrong3_"^short_name) short_name in

  match r1,r2,r3,r4,r5,r6 with
    | true,true,true,false,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

(* 3 *)
type 'a correct1_p3 = (bool ref -> bool) -> bool
type 'a wrong1_p3 = (int ref -> bool) -> bool
type 'a wrong2_p3 = (bool ref -> int) -> bool
type 'a wrong3_p3 = (bool ref -> bool) -> int
type 'a wrong4_p3 = (float -> bool) -> bool

let ex3 =
  let long_name, short_name = "Exercise 3 ", "p3" in
  let r1 = compatible_type ("correct1_"^short_name) short_name in
  let r2 = compatible_type ("wrong1_"^short_name) short_name in
  let r3 = compatible_type ("wrong2_"^short_name) short_name in
  let r4 = compatible_type ("wrong3_"^short_name) short_name in
  let r5 = compatible_type ("wrong4_"^short_name) short_name in

  match r1,r2,r3,r4,r5 with
    | true,false,false,false,false -> correct_answer long_name short_name
    | _  -> wrong_answer long_name short_name

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ ex1; ex2; ex3]
