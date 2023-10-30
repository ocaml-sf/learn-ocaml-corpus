
open Test_lib
open Report
open Parsetree
open Longident

let testint name =
   Section ([ Code name ], test_variable_against_solution [%ty : int] name)

let testexn name =
   Section ([ Code name ], test_variable_against_solution [%ty : exn] name)

let sample_pos () = abs(sample_int())

let test_construct c (e : expression) = match e.pexp_desc with
| Pexp_construct (id, _) when id.txt = Lident c -> true
| _ -> false

(* checks that the number of occurrences a construct in code_ast is n *)
(* status is usually Warning or Failure
    no report is done if the constraints are satisfied *)
let check_one_count name f n msg status =
  let count = ref 0 in
  let reports = find_binding code_ast name (ast_check_expr ~on_expression:
    (fun v -> if f v then incr count; []))
  in
  if !count <> n then [Message ([Text msg], status)]
  else []

(* hide successful tests and only keep the first failure*)
let rec hide l msg = match l with
  | [] -> [Message ([Text msg], Success 1)] (* no failure *)
  | h :: Message (txt, Failure) :: t -> [h; Message (txt, Failure)] (* only keep the first failure with its previous message *)
  | h :: t -> hide t msg

let check_count name l test status=
    Section ([Code name], test @
    if l = [] then []
    else hide (List.concat(List.map (fun (f, n) -> check_one_count name f n "Simplify more." status) l)) "Simplified enough.")

let bools = [true; false]

let prod_bools l = List.map2 (fun x y -> (x, y)) bools l
let bools2 = prod_bools bools
let bools3 = prod_bools bools2

let test_string s (e : expression) = match e.pexp_desc with
| Pexp_constant (Pconst_string (s', _, _)) when s' = s -> true
| _ -> false

let exercise =
  [testint "phrase1";
   testexn "phrase2";
   testexn "phrase3";

   check_count "simplify1" [(test_construct "true", 0); (test_construct "false", 0)]
    (test_function_1_against_solution [%ty : int -> bool] ~gen:0 "simplify1" [-1; 0; 1; 2; 100]) Failure;
   check_count "simplify2" [(test_construct "true", 0); (test_construct "false", 0)]
    (test_function_2_against_solution [%ty : bool -> bool -> bool] ~gen:0 "simplify2" bools2) Failure;
   check_count "edt" [(test_string "Nothing interesting" , 1)]
    (test_function_2_against_solution [%ty : string -> int -> string] ~gen:5 "edt"
        [("monday", 13*60+29); ("monday", 13*60+30); ("monday", 15*60+30); ("monday", 14);
          ("thursday", 8*60+29); ("thursday", 8*60+30); ("thursday", 8*60+31); ("thursday", 10 * 60 + 30)]) Failure;

   check_count "approximately" []
    (test_function_1_against_solution [%ty : int -> string] ~gen:10
    ~sampler: sample_pos "approximately"
            [24*3600; 0; 24*3600 + 1; 24*3600; 3600 +1; 3600 -1; 3600; 59; 60; 61]) Failure;

   check_count "approximately_bonus" []
    (test_function_1_against_solution [%ty : int -> string] ~gen:10
    ~sampler: sample_pos "approximately_bonus"
        [24*3600; 0; 24*3600 + 1; 24*3600; 3600 +1; 3600 -1; 3600; 59; 60; 61]) Failure;

  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

