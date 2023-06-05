open Test_lib
open Report


let sampler_manhattan () =
  let x = Random.int 200 in
  let y = Random.int 200 in

  (* 25% de chance de x = a *)
  let a = if (Random.int 100) < 25 then x else Random.int 200 in
  (* 25% de chance de y = b *)
  let b = if (Random.int 100) < 25 then y else Random.int 200 in
  x,y,a,b

let test_valid_with_solution = 
  Section([Text "Valid tests"; Code("manhattan_distance")],
          test_function_4_against_solution
          [%ty: int -> int -> int -> int -> int] 
          "manhattan_distance"
          ~sampler: (fun () -> sampler_manhattan ())
          ~gen:39
          [(0,0,0,0)]
    )


let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [test_valid_with_solution]