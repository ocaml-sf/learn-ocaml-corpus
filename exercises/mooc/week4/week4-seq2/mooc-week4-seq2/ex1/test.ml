open Report
open Test_lib

let sampler sample =
  let list = sample_list ~min_size: 2 ~max_size: 5 sample in
  sample_alternatively
    [ (fun () ->
          let prf = list () in
          prf, prf) ;
      (fun () ->
         let prf = list () in
         prf, prf @ list ()) ;
      (fun () ->
         let prf = list () in
         prf @ list (), prf) ;
      (fun () ->
         let prf = list () in
         prf @ list (), prf @ list ()) ;
      ( fun () ->
          list (), list ()) ]

  
let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "equal_on_common" ],
           test_function_2_against_solution
             ~sampler: (sampler sample_string)
             [%ty: string list -> string list -> bool ] "equal_on_common"
             [] @
           test_function_2_against_solution
             ~sampler: (sampler sample_int)
             [%ty: int list -> int list -> bool ] "equal_on_common"
             [])

let rec string_of_lid = function
    Longident.Lident s -> s
  | Ldot (l, s) ->
      (string_of_lid l) ^ "." ^ s
  | Lapply (l1, l2) ->
      (string_of_lid l1) ^ "(" ^ (string_of_lid l2) ^ ")"

let exercise_2 =
  let open Parsetree in
  let open Longident in
  let authorized = [ Lident "&&"; Lident "="; Lident "equal_on_common" ] in
  set_progress "Grading exercise 2.";
  Section ([ Text "Exercise 2: using nested `function`" ],
           if snd (Report.result [ exercise_1 ]) then
             [ Message ([ Text "Please complete exercise 1 first." ], Failure)]
           else
             find_binding code_ast "equal_on_common" @@ fun expr ->
             let report = ast_check_expr
                 ~on_expression:(function
                       { pexp_desc = Pexp_tuple _ } ->
                         [ failure ~message:"You cannot directly compare the two \
                                             lists in the pattern matching!" ]
                     | { pexp_desc = Pexp_match (_, _) } ->
                         [ failure ~message:"You cannot use match!" ]
                     | _ -> [])
                 ~on_function_call:(function
                       { pexp_desc = Pexp_ident {Asttypes.txt = lid } }, _
                       when not @@ List.mem lid authorized ->
                         [ failure ~message:
                             (Format.sprintf
                                "You cannot use an auxiliary function `%s`, except \
                                 `&&`, `=` or `equal_on_common` itself"
                              @@ string_of_lid lid) ]
                     | _, _ -> []) expr in
             if snd (Report.result report) then report else
               [ Message ([ Text "Your function has the expected shape!" ], Success 5)] )

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ]
