open Test_lib
open Report

let random_state = ref (Random.get_state ())
  
let test_with_solution = 
  Section([Text "Tests"],
          test_function_1_against_solution
          [%ty: unit -> string] 
          "speak_vacantly"
          ~before_reference: (fun _ -> random_state := Random.get_state ())
          ~before_user: (fun _ ->  Random.set_state !random_state)
          ~gen:20
          ~sampler: (fun () -> ())
          []
    )
    
let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [test_with_solution]
