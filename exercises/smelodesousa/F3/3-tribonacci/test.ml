open Test_lib
open Report

let check_recursion name cb =
  let module Error = struct
    exception RecursionCall
  end in
  find_binding code_ast name @@ fun expr ->
  let contains_recursion_call =
    Parsetree.(
      function
      | { pexp_desc = Pexp_apply ({ pexp_desc = Pexp_ident { txt = id } }, _) }
        ->
          if Longident.last id = name then raise Error.RecursionCall else []
      | _ -> [])
  in
  try
    ast_check_expr ~on_expression:contains_recursion_call expr;
    [
      Message
        ( [
            Text "The function";
            Code name;
            Text "does not contain a recursive call";
          ],
          Failure );
    ]
  with Error.RecursionCall -> cb ()

let test_a_with_solution =
  set_progress "Grading exercise 1";
  Section
    ( [ Text "Exercise 1: "; Code "tribonacci" ],
      check_recursion "tribonacci" @@ fun () ->
      test_function_1_against_solution [%ty: int -> int] "tribonacci"
        ~sampler:(fun () -> Random.int 25)
        ~gen:39 [ 0; -10 ] )

let test_b_with_solution =
  set_progress "Grading exercise 2";
  Section
    ( [ Text "Exercise 2: "; Code "tribonacci_iter" ],
      test_function_1_against_solution [%ty: int -> int] "tribonacci_iter"
        ~sampler:(fun () -> Random.int 50)
        ~gen:39 [ 0; -10 ] )

let test_c_with_solution =
  set_progress "Grading exercise 3";
  Section
    ( [ Text "Exercise 3: "; Code "tribonacci_tail" ],
      check_recursion "tribonacci_tail" @@ fun () ->
      test_function_4_against_solution [%ty: int -> int -> int -> int -> int]
        "tribonacci_tail"
        ~sampler:(fun () -> (Random.int 5000, 1, 1, 1))
        ~gen:39
        [ (0, 1, 1, 1); (5000, 1, 1, 1); (-10, 1, 1, 1); (6, 1, 1, 1) ] )

let () =
  set_result @@ ast_sanity_check code_ast
  @@ fun () ->
  [ test_a_with_solution; test_b_with_solution; test_c_with_solution ]
