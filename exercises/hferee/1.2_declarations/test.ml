open Test_lib
open Report

let testint name =
   Section ([ Code name ], test_variable_against_solution [%ty : int] name)

(* checks that the number of occurrences of function f in code_ast is n *)
let check_count_fun code_ast name f n =
  let count = ref 0 in
  let reports = find_binding code_ast name (ast_check_expr ~on_variable_occurence:
    (fun v -> if v = f then incr count; []))
  in
  if !count <> n then [Message ([Text ("You have to use exactly " ^ string_of_int n ^ " times '" ^ f ^ "'.")], Failure)]
  else [Message ([Text("Correct number of '" ^ f ^ "' : " ^ string_of_int n ^ ".")], Success 1)]


let testmult name =
    Section ([Code name],
    test_variable_against_solution [%ty : int] name @
    check_count_fun code_ast name "*" 3)

let testadd name =
    Section ([Code name],
    test_variable_against_solution [%ty : int] name @
    check_count_fun code_ast name "+" 0)

let exercise =
  [ testadd "phrase0";
    testadd "phrase1";
    testadd "phrase2";
    testadd "phrase3";
    testmult "phrase4"]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

