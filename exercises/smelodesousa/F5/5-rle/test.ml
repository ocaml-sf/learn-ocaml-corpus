open Test_lib
open Report

let rec sample_smal_list x n = 
match n with
0 -> []
| _ -> x::(sample_smal_list x (n-1)) 

let sample_code () =
  let () = Random.self_init () in 
  let res = ref [] in
  let () = 
    for i=0 to 21 do
      res  := (!res@(sample_smal_list (Random.int 10) ((Random.int 3) + 1)))
    done in  !res

let sample_decode () =
  let () = Random.self_init () in   
  let res = ref [] in
  let () = 
    for i=0 to 21 do
      let n = ((Random.int 3) + 1) in
        match n with 
        | 1 -> res  := (One (Random.int 10))::!res
        | _ -> res  := (Many ((Random.int 10), n))::!res
    done in  !res

let ex1 = 
  set_progress "Grading exercise 1" ;
  Section ([ Text "Exercise 1: " ; Code "" ],
           test_variable_against_solution
            [%ty: choice] 
            "p1")

let rle_encodeS = 
  set_progress "Testing encoder function" ;
  Section 
  (
    [ Text "Encoder: " ; Code "cases" ],
    test_function_1_against_solution
      [%ty: int list -> rle_contents list] 
      "rle_encode"
      ~sampler: sample_code
      ~gen: 6
      [[]; [1]; [1;1;1;5]; [5;1;1;1]]
  )

let rle_decodeS = 
  set_progress "Testing decoder function" ;
  Section 
  (
    [ Text "Decoder: " ; Code "cases" ],
    test_function_1_against_solution
      [%ty: rle_contents list -> int list] 
      "rle_decode"
      ~sampler: sample_decode
      ~gen: 7
      [[]; [One (0)]; [Many(1,5)]]
  )

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ ex1; rle_encodeS; rle_decodeS ]