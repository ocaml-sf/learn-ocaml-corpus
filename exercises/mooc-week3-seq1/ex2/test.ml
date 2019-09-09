open Report
open Test_lib

let sample_int () = Random.int 80
let odd () = Random.int 13 * 2 + 1
let even () = Random.int 13 * 2

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "mem" ],
           test_function_2_against_solution
             ~sampler:
               (sample_alternatively
                  [ (fun () ->
                        let l = sample_array ~min_size: 2 ~max_size: 10 ~dups: false odd () in
                        l.(Random.int (Array.length l)), Array.to_list l) ;
                    (fun () ->
                       if Random.bool () then
                         (even (), sample_list ~min_size: 2 ~max_size: 10 ~dups: false  odd ())
                       else
                         (odd (), sample_list ~min_size: 2 ~max_size: 10 ~dups: false  even ())) ])
             [%ty: int -> int list -> bool] "mem"
             [ Random.int 7, [] ])
let sample_list sampler () =
  Test_lib.sample_list ~min_size: 1 ~max_size: 5 sampler ()

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "append" ],
           test_function_2_against_solution
             [%ty: int list -> int list -> int list] "append"
             [])

let sample_list sampler () =
  Test_lib.sample_list ~min_size: 5 ~max_size: 5 sampler ()

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "combine" ],
           test_function_2_against_solution
             [%ty: int list -> int list -> (int * int) list] "combine"
             [([],[]) ; ([1],[2]) ; ([1;2;3], [0;0;0])])

type entry = string * int

let sample_string =
  sample_cases
    [ "let" ; "rec" ; "sig" ; "struct" ;
      "ocp" ; "module" ; "object" ; "begin" ;
      "as" ; "when" ; "for" ; "if" ; "do" ;
      "done" ; "mod" ; "and" ; "match" ]

let sample_other_string =
  sample_cases
    [ "end" ; "sig" ; "val" ; "method" ;
      "include" ; "open" ; "mutable" ; "with" ]

let sample_entry () = (sample_string (), sample_int ())

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "assoc" ],
           test_function_2_against_solution
             ~sampler:
               (let sampler_some () =
                  let l = Test_lib.sample_array ~min_size: 2 ~max_size: 6 ~dups: false sample_entry () in
                  Array.to_list l,
                  fst l.(Random.int (Array.length l)) in
                let sampler_none () =
                  Test_lib.sample_list ~min_size: 2 ~max_size: 6 ~dups: false  sample_entry (),
                  sample_other_string () in
                sample_alternatively
                  [ sampler_some ;
                    sampler_some ;
                    sampler_some ;
                    sampler_none ])
             [%ty: entry list -> string -> int option] "assoc"
             [])

let () =
  set_result @@
  ast_sanity_check ~modules: [ "List" ; "ListLabels" ] code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ]
