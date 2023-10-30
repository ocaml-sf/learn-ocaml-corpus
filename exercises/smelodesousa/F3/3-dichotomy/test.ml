open Test_lib
open Report
open List
open Random
open Solution

(*
   check_recursion name cb

     val name: string

   Checks if function name is recursive. Check_recursion checks
     if there's a function call to name inside the function name.
*)
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

let vector_gen () =
  let () = Random.self_init () in
  let length = Random.int 3 + 14 in
  let v = Array.make length 0 in
  for i = 0 to length - 1 do
    v.(i) <- Random.int 4 + ((2 * i * i) + i)
  done;
  v

let get_value () =
  let () = Random.self_init () in
  let vec = vector_gen () in
  if Random.int 10 >= 3 then
    ( vec.(6 + Random.int (Array.length vec - 8)),
      vec,
      6,
      Array.length vec - 1 - 2 )
  else (900, vec, 0, Array.length vec)

let get_value2 () =
  let () = Random.self_init () in
  let vec = vector_gen () in
  if Random.int 10 >= 3 then (vec.(Random.int (Array.length vec - 1)), vec)
  else (900, vec)

let test_1 =
  set_progress "Grading exercise 1";
  Section
    ( [ Text "Exercise 1: "; Code "binsearch_aux" ],
      check_recursion "binsearch_aux" @@ fun () ->
      test_function_4_against_solution
        [%ty: int -> int array -> int -> int -> int] "binsearch_aux"
        ~sampler:get_value ~gen:10
        [ (12, [| 1; 2; 5; 7; 12; 16; 23; 33; 78 |], 2, 6); (1, [| 1 |], 1, 1) ]
    )

let testT =
  set_progress "Grading exercise 2";
  Section
    ( [ Text "Exercise 2: "; Code "binsearch" ],
      test_function_2_against_solution [%ty: int -> int array -> int]
        "binsearch" ~sampler:get_value2 ~gen:10
        [ (12, [| 1; 2; 5; 7; 12; 16; 23; 33; 78 |]); (1, [| 1 |]) ] )

let () = set_result @@ ast_sanity_check code_ast @@ fun () -> [ test_1; testT ]
