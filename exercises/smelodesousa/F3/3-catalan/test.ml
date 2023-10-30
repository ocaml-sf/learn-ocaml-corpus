open Test_lib
open Report

let check_recursion name cb =
  let module Error = struct exception RecursionCall end in

  find_binding code_ast name @@ fun expr ->
    let contains_recursion_call = Parsetree.(function
      | {pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt = id}}, _)} -> 
        if (Longident.last id) = name then raise Error.RecursionCall else []
      | _ -> []) in
    try
      ast_check_expr ~on_expression:contains_recursion_call expr;
      [Message ([Text "The function"; Code name; Text "does not contain a recursive call"], Failure)]
    with Error.RecursionCall -> cb ()
  
let int_sampler () = 
  let () = Random.self_init () in
    ((Random.int 31) - 15)

let catalanS () = 
    check_recursion "catalan" @@ fun () -> test_function_1_against_solution
    [%ty: int -> int ]
    "catalan"
    ~sampler: int_sampler
    ~gen: 9
    [ -100; 0 ]

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  catalanS ()