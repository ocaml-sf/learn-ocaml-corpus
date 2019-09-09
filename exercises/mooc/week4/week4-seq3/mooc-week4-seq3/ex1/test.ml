open Report
open Test_lib

let eq_floats f1 f2 = abs_float (f2 -. f1) < 0.00001
let test_eq_floats = test_eq_ok eq_floats

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "ccr" ],
           test_function_4_against_solution ~test:test_eq_floats
             [%ty: float -> float -> float -> float -> float] "ccr"
             [])

let test_ccr () =
  test_variable_property
    [%ty: float -> float -> float -> float -> float] "ccr" @@ fun ccr_f ->
  let comp_a, comp_b, comp_c, comp_s, total =
    (* Random.float just to be cautious in case the compiler gets super smart *)
    test_ccr ccr_f (Random.float 1.0, Random.float 2.0, Random.float 3.0, Random.float 4.0) in
  let res_a =
    if not comp_a then
      Message
        ([Text "The partial application of " ; Code "a" ; Text " can compute more."],
         Failure)
    else
      Message
        ([Text "The partial application of " ; Code "a" ; Text " is optimized."],
         Success 5)
  in
  let res_b =
    if not comp_b then
      Message
        ([Text "The partial application of " ; Code "b" ; Text " can compute more."],
         Failure)
    else
      Message
        ([Text "The partial application of " ; Code "b" ; Text " is optimized."],
         Success 5)
  in
  let res_c =
    if not comp_c then
      Message
        ([Text "The partial application of " ; Code "c" ; Text " can compute more."],
         Failure)
    else
      Message
        ([Text "The partial application of " ; Code "c" ; Text " is optimized."],
         Success 5) in
  let res_s =
    if not comp_s then
      Message
        ([Text "The final application of " ; Code "s" ; Text " can compute more."],
         Failure)
    else
      Message
        ([Text "The final application of " ; Code "s" ; Text " is optimized."],
         Success 5) in
  [ res_a; res_b; res_c; res_s ] @
  if total > 10 then
    [ Message ([ Text (string_of_int total ^ " operations is too much!") ], Failure) ;
      Message ([ Text "I'm sure you can cut down to 10 operations!" ], Important) ]
  else
    [ Message ([Text "Well done, you made it down to 10 operations."], Success 5) ]

let exercise_2 =
  set_progress "Grading exercise 2.";
  Section ([ Text "Exercice 2: partial applications" ], test_ccr ())

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ]
