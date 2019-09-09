open Report
open Test_lib

let sample_string =
  let sample_one = sample_cases [ "Ro" ; "Ra" ; "Ya" ; "Gre" ; "Ca" ; "Benja" ] in
  let sample_two = sample_cases [ "berto" ; "lf" ; "nn" ; "goire" ; "gdas" ; "min" ] in
  fun () -> sample_one () ^ sample_two ()

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ Section ([ Text "Exercise 1:" ; Code "last_character" ],
             test_function_1_against_solution
               [%ty: string -> char] "last_character"
               []) ;
    Section ([ Text "Exercise 2:" ; Code "string_of_bool" ],
             test_function_1
               [%ty: bool -> string] "string_of_bool"
               [ true, "true", "", "" ; false, "false", "", ""]) ]
