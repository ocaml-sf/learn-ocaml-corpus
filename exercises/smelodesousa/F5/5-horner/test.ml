open Test_lib
open Report
open List
open Random

exception SeenLoops

let polynomial_gen () =
  let l =
    sample_list ~min_size:3 ~max_size:10
      (fun () ->
        let () = Random.self_init () in
        Random.float 10.)
      ()
  in
  let n = List.length l - 1 in
  (n, l)

let polynomial_tr_gen () = (polynomial_gen (), (-1, []))

let hornerR =
  Section
    ( [ Text "Testing function"; Code "horner" ],
      test_function_2_against_solution [%ty: float -> int * float list -> float]
        "horner"
        ~sampler:(fun () -> (Random.float 10., polynomial_gen ()))
        ~gen:10
        [
          (3.0, (4, [ 3.; 0.; 5.; 0.; 1. ]));
          (3.0, (-1, [ 1. ]));
          (3.0, (0, []));
          (3.0, (-1, []));
        ] )

let derivativeA =
  Section
    ( [ Text "Testing function"; Code "derivative" ],
      test_function_1_against_solution
        [%ty: int * float list -> int * float list] "derivative"
        ~sampler:polynomial_gen ~gen:10
        [ (4, [ 3.; 0.; 5.; 0.; 1. ]); (-1, [ 1. ]); (0, []); (-1, []) ] )

let failWith msg = [ Message ([ Text msg ], Failure) ]

let checkForLoops cb =
  find_binding code_ast "derivative_tr" @@ fun expr ->
  let contains_loops =
    Parsetree.(
      function
      | { pexp_desc = Pexp_for _ } | { pexp_desc = Pexp_while _ } ->
          raise SeenLoops
      | _ -> [])
  in
  try
    ast_check_expr ~on_expression:contains_loops expr;
    cb ()
  with SeenLoops ->
    failWith
      "Loops are not allowed on this exercise! Implement a recursive version."

let derivative_trT =
  Section
    ( [ Text "Testing function"; Code "derivative_tr" ],
      test_function_2_against_solution
        [%ty: int * float list -> int * float list -> int * float list]
        "derivative_tr" ~sampler:polynomial_tr_gen ~gen:10
        [
          ((4, [ 3.; 0.; 5.; 0.; 1. ]), (-1, []));
          ((-1, [ 1. ]), (-1, []));
          ((0, []), (-1, []));
          ((-1, []), (-1, []));
        ] )

let choiceT =
  Section
    ( [ Text "Testing variable answer" ],
      test_variable_against_solution [%ty: choice] "answer" )

let () =
  set_result @@ ast_sanity_check code_ast
  @@ fun () ->
  checkForLoops @@ fun () -> [ hornerR; derivativeA; derivative_trT; choiceT ]
