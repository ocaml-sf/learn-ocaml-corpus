open Report
open Test_lib

type int_ff = int -> int
let sample_int_ff =
  sample_cases
    [ printable_fun "((+) 10)" ((+) 10) ;
      printable_fun "((-) 7)" ((-) 7) ;
      printable_fun "((/) 4)" ((/) 4) ]

type string_ff = string -> string
let sample_string_ff =
  sample_cases
    [ printable_fun "((^) \"@\")" ((^) "@") ;
      printable_fun "(fun s -> s ^ s)" (fun s -> s ^ s) ;
      printable_fun "String.uppercase" (String.uppercase) ]

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "compose" ],
           test_function_2_against_solution ~gen: 5
             [%ty: int_ff list -> int -> int] "compose"
             [] @
           test_function_2_against_solution ~gen: 5
             [%ty: string_ff list -> string -> string] "compose"
             [])

type float_ff = float -> float
let sample_float_ff =
  sample_cases
    [ printable_fun "cos" cos ;
      printable_fun "sin" sin ;
      printable_fun "(fun _ -> 10.)" (fun _ -> 10.) ;
      printable_fun "((*.) 0.1)" (( *. ) 0.1) ]

type delta = float
let sample_delta () = Random.float 0.09 +. 0.01

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "fixedpoint" ],
           test_function_3_against_solution
             [%ty: float_ff -> float -> delta -> float ] "fixedpoint"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ]
