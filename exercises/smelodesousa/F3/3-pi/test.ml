open Test_lib
open Report

let factor_sampler () =
  let () = Random.self_init () in
  Random.int 1000

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

let aproximateS =
  set_progress "Grading exercise";
  Section
    ( [ Text "Testing function"; Code "approximate_pi" ],
      check_recursion "approximate_pi" @@ fun () ->
      test_function_1_against_solution [%ty: int -> float] "approximate_pi"
        ~sampler:factor_sampler ~gen:8 [ -100; 0; 10001 ] )

let () = set_result @@ ast_sanity_check code_ast @@ fun () -> [ aproximateS ]
