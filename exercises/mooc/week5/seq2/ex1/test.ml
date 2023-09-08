open Report
open Test_lib

let sample_int = sample_int

let exercise_1 =
  Section ([ Text "Exercise 1: " ; Code "print_int_list" ],
           test_function_1_against_solution
             ~test: test_ignore
             ~test_stdout: (io_test_items ~split: ['\n'] ~trim:[' '] ~skip_empty: true)
             [%ty: int list -> unit] "print_int_list"
             [])

let sample_int () =
  Random.int 5 + 1

type int_list = int list

let sample_int_list () =
  sample_list (fun () -> Random.int 20 - 10) ()

let exercise_2 =
  Section ([ Text "Exercise 2: " ; Code "print_every_other" ],
           test_function_2_against_solution
             ~test: test_ignore
             ~test_stdout: (io_test_items ~split: ['\n'] ~trim:[' '] ~skip_empty: true)
             [%ty: int -> int_list -> unit] "print_every_other"
             [])

type int_printer = int -> unit
let sample_int_printer =
  sample_cases
    [ printable_fun "print_int" (fun i -> print_int i ; flush stdout) ;
      printable_fun "(Printf.printf \"[%d]\")" (Printf.printf "[%d]%!") ]

type bool_printer = bool -> unit
let sample_bool_printer =
  sample_cases
    [ printable_fun
        "(function true -> print_string \"YES\" | false -> print_string \"NO\")"
        (function
          | true -> print_string "YES" ; flush stdout
          | false -> print_string "NO" ; flush stdout) ;
      printable_fun "(Printf.printf \"%b\")" (Printf.printf "%b%!") ]

let exercise_3 =
  Section ([ Text "Exercise 3: " ; Code "print_list" ],
           [ Message ([ Text "testing with integers" ], Important) ] @
           test_function_2_against_solution
             ~test: test_ignore
             ~test_stdout: (io_test_items ~split: ['\n'] ~trim:[' '] ~skip_empty: true)
             [%ty: int_printer -> int list -> unit] "print_list"
             [] @
           [ Message ([ Text "testing with booleans" ], Important) ] @
           test_function_2_against_solution
             ~test: test_ignore
             ~test_stdout: (io_test_items ~split: ['\n'] ~trim:[' '] ~skip_empty: true)
             [%ty: bool_printer -> bool list -> unit] "print_list"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ]
