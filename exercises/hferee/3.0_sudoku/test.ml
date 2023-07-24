
open Test_lib
open Report

(* find_binding ne marche pas avec les annotations de type *)
let find_binding code_ast name cb =
  let open Parsetree in
  let open Learnocaml_report in
  let rec findlet = function
    | [] -> [ Message ([ Text "I could not find " ; Code name ; Text "." ;
                         Break ;
                         Text "Check that it is defined as a simple " ; Code "let" ;
                         Text " at top level." ], Failure) ]
    | { pstr_desc = Pstr_value (_, bds); _ } :: rest ->
       let rec findvar = function
         | [] -> findlet rest
         | { pvb_pat = {
              ppat_desc = Ppat_constraint (
                { ppat_desc = Ppat_var { Location.txt; _ }; _} , _) ;
              _ } ;
             pvb_expr; _ } :: _
         | { pvb_pat = {
              ppat_desc = Ppat_var { Location.txt; _ };
              _ };
             pvb_expr; _ } :: _ when txt = name
            -> Message ([ Text "Found a toplevel definition for " ; Code name ; Text "."], Informative)
            :: cb pvb_expr
         | _ :: rest -> findvar rest in
       findvar bds
    | _ :: rest -> findlet rest
  in findlet (List.rev code_ast)

(* interdire le filtrage de motifs *)
let nopat fname =
  let rec forbid_pat (p : Parsetree.pattern) = match p.ppat_desc with
    | Ppat_var v -> [] (* nommer des variables c'est ok *)
    | Ppat_constraint (p, _) -> forbid_pat p (* annoter des types, on verra *)
    | _ -> [Message([Text ("Do not use pattern matching inside " ^ fname) ], Failure)] in
  find_binding code_ast fname (ast_check_expr ~on_pattern:forbid_pat)

let nopatatall fname =
  let msg = [Message([Text ("Don't use arguments to define " ^ fname) ], Failure)] in
  find_binding code_ast fname (ast_check_expr ~on_pattern: (fun _ -> msg))

(* 0 is left on purpose to make bad grilles*)
let sc () = (abs(sample_int()) mod 10)
let triple f () = f(), f(), f()
let sample_small_line = triple sc
let sample_block = triple sample_small_line
let sample_gl = triple sample_block
let sample_grid = triple sample_gl

let sample_s3 () = sample_string(), sample_string(), sample_string()
let good_block = ((1, 2, 3), (4, 5, 6), (7, 8, 9))
let good_gl = ((1, 2, 3), (4, 5, 6), (7, 8, 9)), ((7, 8, 9), (1, 2, 3), (4, 5, 6)), ((4, 5, 6), (7, 8, 9), (1, 2, 3))
let good_grid = (((4, 1, 5), (3, 6, 2), (7, 8, 9)),
                   ((6, 3, 8), (4, 7, 9), (2, 1, 5)),
                   ((9, 7, 2), (1, 6, 5), (3, 6, 4))),
                  (((9, 2, 6), (1, 3, 8), (5, 7, 4)),
                  ((3, 4, 1), (7, 5, 6), (9, 8, 2)),
                  ((7, 5, 8), (4, 2, 9), (6, 3, 1))),
                  (((2, 5, 7), (8, 4, 3), (6, 9, 1)),
                   ((1, 6, 4), (5, 9, 7), (8, 2, 3)),
                   ((8, 9, 3), (2, 1, 6), (5, 4, 7))
                  )

let sample3 () = sample_string(), sample_int(), sample_float()
let exercise =
  [
    Section([Code "Question 1"],
      test_function_1_against_solution [%ty : string * int * float -> string]
        ~gen:0 "p1" ["0", 0, 0.] @
      test_function_1_against_solution [%ty : string * int * float -> int]
        ~gen:0 "p2" ["0", 0, 0.] @
      test_function_1_against_solution [%ty : string * int * float -> float]
        ~gen:0 "p3" ["0", 0, 0.] @
      test_function_2_against_solution [%ty : (int -> string)  -> (int * int * int) -> (string * string * string) ]
        ~gen:0 "app3" [(string_of_int, (1, 2, 3))] @
      test_function_2_against_solution [%ty : (string -> bool) -> (string * string * string) -> bool]
        ~gen:0 "all" [(=) "ok", ("", "", ""); (=) "ok", ("ok", "ok", "ok"); (=) "ok", ("", "", "ok")] @
      test_function_2_against_solution [%ty : (string -> bool) -> (string * string * string) -> bool]
        ~gen:5 "exist" ~sampler: (fun () ->  (fun s -> s < "m"), sample_s3()) [] @
      test_function_2_against_solution [%ty : (int -> int -> bool) -> (int * int * int) -> bool]
      ~gen:0 "exist_pair" [(=), (1, 2, 3); (=), (1, 2, 1); (=), (1, 1, 2); (=), (1, 2, 2)]
    ) ;
    Section([Code "Question 2"],
      test_function_1_against_solution [%ty : int -> bool] ~gen:0 "within_bounds" [0; 1; 9; 10; 11] @
      test_function_1_against_solution [%ty : small_line -> bool] ~gen:3
        "within_bounds_small_line" [(1, 0, 1); (0, 1, 1); (1, 1, 0); (1, 1, 1)] @
      nopatatall "within_bounds_small_line" @
      test_function_1_against_solution [%ty : block -> bool] ~gen:5 ~sampler:
        sample_block "within_bounds_block" [good_block] @
      nopatatall "within_bounds_block" @
      test_function_1_against_solution [%ty : grid -> bool] ~gen:5 ~sampler:
        sample_grid "within_bounds_grid" [good_grid]
    );
    Section([Code "Question 3"],
      test_function_1_against_solution [%ty : small_line -> bool] ~gen:3
        "differents_small_line" [(1, 0, 2); (0, 1, 1); (1, 1, 1)] @
      test_function_2_against_solution [%ty : small_line -> int -> bool] ~gen:5
        "within" [((0, 1, 2), 1); ((0, 1, 2), 3)] @
      test_function_2_against_solution [%ty : small_line -> small_line -> bool] ~gen:5
        "intersects" [((0, 1, 2), (1, 3, 4)); ((0, 1, 2), (3, 4, 5))] @
      test_function_1_against_solution [%ty : block -> bool] ~gen:5
        "differents_block" [good_block; ((1, 2, 3), (4, 5, 6), (7, 8, 2)); (1, 2, 3), (4, 5, 4), (7, 8, 9)]
    );
    Section([Code "Question 4"],
     test_function_1_against_solution [%ty : block -> bool] ~gen:5 ~sampler: sample_block "block_correct" [good_block] @
     nopat "block_correct" @
     test_function_1_against_solution [%ty : grid -> bool] ~gen:5 ~sampler: sample_grid "blocks_correct" [good_grid] @
     nopatatall "blocks_correct"
    )

    ;
     Section([Code "Question 5"],
    test_function_1_against_solution [%ty : (string * int * float) * (string * int * float)* (string * int * float) ->
        (string * string * string) * (int * int * int) * (float * float * float)]
      ~gen:3 ~sampler:(fun () -> sample3(), sample3(), sample3()) "transpose9" []);
    Section([Code "Question 6"],
      test_function_1_against_solution [%ty : grid -> grid]
        ~gen:0 "transpose_lines_blocks" [good_grid] @
      nopatatall "transpose_lines_blocks" @
      test_function_1_against_solution [%ty : grid -> bool]
        ~gen:5 ~sampler: sample_grid "lines_correct" [good_grid] @
      nopat "lines_correct"
    );
    Section([Code "Question 7"],
    test_function_1_against_solution [%ty : grid -> grid]
      ~gen:0 "transpose_blocks" [good_grid] @
    nopatatall "transpose_blocks" @
    test_function_1_against_solution [%ty : grid -> grid]
      ~gen:0 "transpose_grid" [good_grid] @
    nopat "transpose_grid" @
    test_function_1_against_solution [%ty : grid -> bool]
      ~gen:5 ~sampler: sample_grid "columns_correct" [good_grid] @
    nopat "columns_correct" @
    test_function_1_against_solution [%ty : grid -> bool]
      ~gen:5 ~sampler: sample_grid "correct" [good_grid] @
    nopat "correct"
    )
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise
