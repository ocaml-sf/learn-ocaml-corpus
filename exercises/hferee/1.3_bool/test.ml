open Test_lib
open Report

let testint name =
   Section ([ Code name ], test_variable_against_solution [%ty : int] name)

(* checks that the number of occurrences of function f in code_ast is n *)
(* status is usually Warning or Failure
    no report is done if the constraints are satisfied *)
let check_one_count name f n msg status =
  let count = ref 0 in
  let reports = find_binding code_ast name (ast_check_expr ~on_variable_occurence:
    (fun v -> if v = f then incr count; []))
  in
  if !count <> n then [Message ([Text msg], status)]
  else []

(* hide successful tests and only keep the first failure*)
let hide l msg = match List.filter (fun m -> match m with | Message (_, Failure _) -> true | _ -> false) l with
  | [] -> [Message ([Text msg], Success 1)] (* no failure *)
  | h :: t -> [h] (* only keep the first one *)

let check_count name l test status=
    Section ([Code name],
    hide test "Correct value on a set of tests" @
    hide (List.concat(List.map (fun (f, n) -> check_one_count name f n "Simplify more." status) l)) "Simplified enough.")

let bools = [true; false]

let prod_bools l = List.map (fun x -> (true, x)) l @
                   List.map (fun x -> (false, x)) l
let bools2 = prod_bools bools
let bools3 = prod_bools bools2


let exercise =
  [
    Section ([Code "interval10"], hide (test_function_1_against_solution [%ty : int -> bool] "interval10" ~gen:0 [-1; 0;  5; 9; 10; 11; 42]) "correct value");
    check_count "value1" [("&&", 0); ("||", 0); ("not", 0)] (test_variable_against_solution [%ty : bool] "value1") Failure;
    check_count "value2" [("&&", 0); ("||", 0); ("not", 0)] (test_variable_against_solution [%ty : bool] "value2") Failure;
    check_count "value3" [("&&", 0); ("||", 0); ("not", 0)] (test_variable_against_solution [%ty : bool] "value3") Failure;

    check_count "simplify1" [("&&", 0); ("||", 1); ("not", 0)]
        (test_function_2_against_solution [%ty : bool -> bool -> bool] ~gen:0 "simplify1" bools2) Failure;
    check_count "simplify2" [("&&", 0); ("||", 0); ("not", 0)]
        (test_function_2_against_solution [%ty : int -> int -> bool] ~gen:10 "simplify2" [5, 6]) Failure;

    check_count "simplify3" [("&&", 1); ("=", 2); ("||", 0)]
        (test_function_3_against_solution [%ty : int -> int -> int -> bool] ~gen:20 "simplify3" []) Failure;
    check_count "simplify4" [("&&", 0); ("||", 1); ("not", 0)]
        (test_function_2_against_solution [%ty : int -> int -> bool] ~gen:10 "simplify4" [(7, 2); (7, 3); (8, 2); (8, 3)]) Failure;
    check_count "simplify5" [("&&", 0); ("||", 0); ("not", 0); ("true", 0); ("false", 0); ("=", 0)]
        (test_function_1_against_solution [%ty : bool -> bool] ~gen:0 "simplify5" bools) Failure;
    check_count "leap" [("mod", 0); ("||", 1); ("&&", 1)]
        (test_function_1_against_solution [%ty : int -> bool] ~gen:100 "leap" [0; 4; 100; 200; 400; 700; 800; 804]) Warning
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

