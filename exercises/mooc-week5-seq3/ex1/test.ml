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

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "output_multiples" ],
           find_binding code_ast "output_multiples" @@ fun ast ->
           match lookup_for ast with
           | None ->
               [ Message ([ Text "I can't find the for loop!" ], Failure) ]
           | Some _ ->
               [ Message ([ Text "You used a for loop, bravo!!" ], Success 5) ;
                 Message ([ Text "Now I will check that it behaves correctly" ], Important) ] @
               test_function_3_against_solution
                 ~sampler: (fun () -> Random.int 6 + 1, Random.int 5 + 2, Random.int 10 + 30)
                 ~test: test_ignore
                 ~test_stdout: (io_test_items ~split: [',' ; ';' ; '\n'] ~drop: [' '] ~skip_empty: true)
                 [%ty: int -> int -> int -> unit] "output_multiples"
                 [])

type int_fun = int -> int
let sample_int_fun =
  sample_cases
    [ printable_fun "(fun i -> 8 - i)" (fun i -> 8 - i) ;
      printable_fun "(fun i -> i - 6)" (fun i -> i - 6) ;
      printable_fun "(fun i -> 4 - i)" (fun i -> 4 - i) ;
      printable_fun "(fun i -> i - 3)" (fun i -> i - 3) ;
      printable_fun "(fun i -> (i mod 2) * 2 - 1)" (fun i -> (i mod 2) * 2 - 1) ]

let sample_int () =
  Random.int 5 + 5

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "display_sign_until_zero" ],
           find_binding code_ast "display_sign_until_zero" @@ fun ast ->
           let open Parsetree in
           let checks =
             ast_check_structure
               ~on_structure_item:(function
                   | { pstr_desc = Pstr_exception _ } ->
                       [ Message ([ Text "You defined an exception." ], Success 5) ]
                   | _ -> [])
               code_ast @
             ast_check_expr
               ~on_pattern:(function
                   | { ppat_desc = Ppat_exception _ } ->
                       [ Message ([ Text "You used an exception catcher." ], Success 5) ]
                   | _ -> [])
               ~on_expression:(function
                   | { pexp_desc = Pexp_for _ } ->
                       [ Message ([ Text "You used a" ; Code "for" ; Text "loop." ], Success 5) ]
                   | { pexp_desc = Pexp_try _ } ->
                       [ Message ([ Text "You used an exception catcher." ], Success 5) ]
                   | { pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident { Location.txt = Longident.Lident "raise"} },  _) } ->
                       [ Message ([ Text "You used a" ; Code "raise" ; Text "instruction ." ], Success 5) ]
                   | _ -> [])
               ast
             |> List.sort_uniq compare in
           if List.length checks < 4 then
             [ Message ([ Text "I need a for loop, an exception definition, raising and catching !" ], Failure) ]
           else
             [ Message ([ Text "You used all the required syntactic constructs." ], Informative) ;
               Message ([ Text "Now I will check that your function behaves correctly" ], Important) ] @
             test_function_2_against_solution
               ~test: test_ignore
               ~test_stdout: (io_test_items ~split: [ '\n' ; ' ' ; ';' ; ',' ; '.'] ~skip_empty: true)
               [%ty: int_fun -> int -> unit] "display_sign_until_zero"
               [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ]
