open Test_lib
open Report

exception SeenLoops

let failWith msg =
  [Message ([Text msg], Failure)]

let checkForLoops cb =
  find_binding code_ast "sum3_tr" @@ fun expr ->

  let contains_loops = Parsetree.(function
    | { pexp_desc = Pexp_for _ } | { pexp_desc = Pexp_while _ } -> raise SeenLoops
    | _ -> [])
  in
  try
    ast_check_expr ~on_expression:contains_loops expr;
    cb ()
  with SeenLoops ->
  failWith "Loops are not allowed in this exercise! Propose a tr recursive version"

let int_sampler () = 
  let () = Random.self_init () in
    (Random.int 12)

let int_tr_sampler () = 
  let () = Random.self_init () in
    (Random.int 12, 0)

let sum3S = 
  set_progress "Correcting question 1" ;
  Section ([ Text "Exercise 1: " ; Code "sum3" ],
  test_function_1_against_solution
    [%ty: int -> int ]
    "sum3"
    ~sampler: int_sampler
    ~gen: 9
    [ 0 ])

let sum3_trS = 
  set_progress "Correcting question 2" ;
  Section ([ Text "Exercise 2: " ; Code "sum3_tr" ],
            checkForLoops @@ fun () -> test_function_2_against_solution
              [%ty: int -> int -> int ]
              "sum3_tr"
              ~sampler: int_tr_sampler
              ~gen: 9
              [ (0, 0) ])



let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ sum3S; sum3_trS]
