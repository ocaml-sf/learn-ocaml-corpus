open Report
open Test_lib

let lookup_for ast =
  let open Parsetree in
  let open Ast_mapper in
  let res = ref None in
  try
    let expr mapper = function
      | { pexp_desc = Pexp_for (_, _, _, _, body) } -> res := Some body ; raise Exit
      | p -> default_mapper.expr mapper p in
    let mapper = { default_mapper with expr } in
    ignore (mapper.expr mapper ast) ; !res
  with Exit -> !res

let sample_int () = 10

let sample_image =
  sample_cases
    [ printable_fun "checkers" checkers ;
      printable_fun "all_black" all_black ;
      printable_fun "all_white" all_white ;
      printable_fun "(disk 5 5 5)" (disk 5 5 5) ;
      printable_fun "(disk 3 7 3)" (disk 3 7 3) ;
      printable_fun "(disk 3 7 3)" (disk 3 7 3) ;
      printable_fun "(disk 7 3 3)" (disk 7 3 3) ;
      printable_fun "(disk 5 5 3)" (disk 7 3 3) ;
      printable_fun "(square 5 5 5)" (square 5 5 5) ;
      printable_fun "(square 3 7 3)" (square 3 7 3) ;
      printable_fun "(square 3 7 3)" (square 3 7 3) ;
      printable_fun "(square 7 3 3)" (square 7 3 3) ;
      printable_fun "(square 7 3 3)" (square 7 3 3) ]

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "display_image" ],
           find_binding code_ast "display_image" @@ fun ast ->
           match lookup_for ast with
           | None ->
               [ Message ([ Text "I can't find any for loop!" ], Failure) ]
           | Some ast ->
               match lookup_for ast with
               | None ->
                   [ Message ([ Text "I can't find the inner for loop!" ], Failure) ]
               | Some ast ->
                   [ Message ([ Text "You used a two nested loops, bravo!!" ], Success 5) ;
                     Message ([ Text "Now I will check that it behaves correctly" ], Important) ] @
                   test_function_3_against_solution
                     ~test: test_ignore
                     ~test_stdout: (io_test_items ~split: ['\n'] ~trim:[] ~skip_empty: true)
                     [%ty: int -> int -> image -> unit] "display_image"
                     [])

let sample_image =
  sample_cases
    [ printable_fun "checkers" checkers ;
      printable_fun "all_black" all_black ;
      printable_fun "(disk 5 5 5)" (disk 5 5 5) ;
      printable_fun "(square 5 5 5)" (square 5 5 5) ]

let rec sample_blend () =
  let rec sample level =
    match Random.int level with
    | 0 -> Image (sample_image ())
    | _ -> match Random.int 3 with
      | 0 -> And (sample (level - 1), sample (level - 1))
      | 1 -> Or (sample (level - 1), sample (level - 1))
      | _ -> Rem (sample (level - 1), sample (level - 1)) in
  let res = sample 4 in
  let black = ref true in
  let white = ref true in
  for y = 0 to 10 do
    for x = 0 to 10 do
      if Solution.render res x y then black := false else white := false
    done
  done ;
  if !black || !white then sample_blend () else res


let sample_int () =
  Random.int 2 + 6

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "render" ],
           test_function_3_against_solution ~gen: 20
             [%ty: blend -> int -> int -> bool] "render"
             [])

let sample_int () = 10

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "display_blend" ],
           test_function_3_against_solution ~gen: 20
             ~test: test_ignore
             ~test_stdout: (io_test_items ~split: ['\n'] ~trim:[] ~skip_empty: true)
             [%ty: int -> int -> blend -> unit] "display_blend"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ]
