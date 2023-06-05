
open Test_lib
open Report
open Parsetree
open Longident

let test_construct c (e : expression) = match e.pexp_desc with
| Pexp_construct (id, _) | Pexp_ident (id) when id.txt = Lident c -> true
| _ -> false

let check name f n =
  let count = ref 0 in
  let reports = find_binding code_ast name (ast_check_expr ~on_expression:
    (fun v -> if test_construct f v then incr count; []))
  in
  if !count <> n then
    let msg = if n = 0 then "Ne pas utiliser " else "Utiliser " in
    [Message ([Text (msg ^ f)], Failure)]
  else []

let test name t constraints =
  Section([Code name], t @
          List.fold_left (fun acc f -> acc @ check name f 1) [] constraints @
                       (* ne jamais utiliser les constructeurs de type *)
          List.fold_left (fun acc f -> acc @ check name f 0) [] ["Nothing"; "OneMore"]
         )

let test_list =  OneMore(0, OneMore(1, OneMore(min_int, OneMore(max_int, OneMore(2, Nothing)))))
let test_fun = fun x -> x mod 2 = 0
let exercise =
  [
    test "list_min"
      (test_function_1_against_solution ~gen:0 [%ty : liste -> int] "list_min"
         [Nothing; test_list])
      ["min"; "fold_right"];

    test "list_max"
      (test_function_1_against_solution  ~gen:0 [%ty : liste -> int]
         "list_max" [Nothing; test_list])
      ["max"; "fold_right"];
    test "count_if"
      (test_function_2_against_solution  ~gen:0 [%ty : (int -> bool)
         -> liste -> int] "count_if"
        [test_fun, Nothing; test_fun, test_list])
      ["fold_right"];
    test "forall"
      (test_function_2_against_solution  ~gen:0 [%ty : (int -> bool) -> liste -> bool] "forall"
      [test_fun, Nothing; test_fun, test_list; (<>) 4, test_list])
      ["fold_right"];
    test "exists"
      (test_function_2_against_solution  ~gen:0 [%ty : (int -> bool) -> liste -> bool] "exists"
      [test_fun, Nothing; test_fun, test_list; (=) 4, test_list])
      ["fold_right"];
    test "mem"
      (test_function_2_against_solution ~gen:0 [%ty : int -> liste -> bool] "mem"
      [0, Nothing; 0, test_list; 4, test_list])
      ["exists"];
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

(*
let list_min l

let list_max l

count_if p l

forall p l

exists p l

mem x

(* bonus *)
filter f l
map f l
find_first p l

find_last p l
   *)

