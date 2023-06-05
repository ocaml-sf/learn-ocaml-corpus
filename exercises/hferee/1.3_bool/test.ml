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
    hide test "Valeur correcte sur un ensemble de tests" @
    hide (List.concat(List.map (fun (f, n) -> check_one_count name f n "Simplifier davantage." status) l)) "Suffisamment simplifié.")

let bools = [true; false]

let prod_bools l = List.map (fun x -> (true, x)) l @
                   List.map (fun x -> (false, x)) l
let bools2 = prod_bools bools
let bools3 = prod_bools bools2


let exercise =
  [
    Section ([Code "intervalle10"], hide (test_function_1_against_solution [%ty : int -> bool] "intervalle10" ~gen:0 [-1; 0;  5; 9; 10; 11; 42]) "valeur correcte");
    check_count "valeur1" [("&&", 0); ("||", 0); ("not", 0)] (test_variable_against_solution [%ty : bool] "valeur1") Failure;
    check_count "valeur2" [("&&", 0); ("||", 0); ("not", 0)] (test_variable_against_solution [%ty : bool] "valeur2") Failure;
    check_count "valeur3" [("&&", 0); ("||", 0); ("not", 0)] (test_variable_against_solution [%ty : bool] "valeur3") Failure;

    check_count "simplifier1" [("&&", 0); ("||", 1); ("not", 0)]
        (test_function_2_against_solution [%ty : bool -> bool -> bool] ~gen:0 "simplifier1" bools2) Failure;
    check_count "simplifier2" [("&&", 0); ("||", 0); ("not", 0)]
        (test_function_2_against_solution [%ty : int -> int -> bool] ~gen:10 "simplifier2" [5, 6]) Failure;

    check_count "simplifier3" [("&&", 1); ("=", 2); ("||", 0)]
        (test_function_3_against_solution [%ty : int -> int -> int -> bool] ~gen:20 "simplifier3" []) Failure;
    check_count "simplifier4" [("&&", 0); ("||", 1); ("not", 0)]
        (test_function_2_against_solution [%ty : int -> int -> bool] ~gen:10 "simplifier4" [(7, 2); (7, 3); (8, 2); (8, 3)]) Failure;
    check_count "simplifier5" [("&&", 0); ("||", 0); ("not", 0); ("true", 0); ("false", 0); ("=", 0)]
        (test_function_1_against_solution [%ty : bool -> bool] ~gen:0 "simplifier5" bools) Failure;
    check_count "bissextile" [("mod", 0); ("||", 1); ("&&", 1)]
        (test_function_1_against_solution [%ty : int -> bool] ~gen:100 "bissextile" [0; 4; 100; 200; 400; 700; 800; 804]) Warning
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

