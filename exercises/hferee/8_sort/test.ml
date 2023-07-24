
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
    let msg = if n = 0 then "Do not use " else "Use " in
    [Message ([Text (msg ^ f)], Failure)]
  else []

let test_list =  OneMore(0, OneMore(1, OneMore(min_int, OneMore(max_int, OneMore(2, Nothing)))))
let sorted_list =  OneMore(min_int, OneMore(-1, OneMore(1, OneMore(7,
                                                                       OneMore(max_int, Nothing)))))
let test name t constraints =
  Section([Code name], t @
          List.fold_left (fun acc f -> acc @ check name f 1) [] constraints
         )

let test_fun = fun x -> x mod 2 = 0
let exercise =
  [
    test "insert"
      (test_function_2_against_solution  [%ty : int -> liste -> liste] ~gen:0
         "insert" [7, Nothing; 3, sorted_list])
      [];

    test "sort"
      (test_function_1_against_solution [%ty : liste -> liste] ~gen:0 "sort"
         [Nothing; test_list])
      ["insert"; "fold_right"]
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

