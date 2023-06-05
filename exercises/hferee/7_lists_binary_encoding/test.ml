
open Test_lib
open Report

let sample_pos () = abs (sample_int())
let sample_bin () = Solution.encode (abs (sample_int()))
let sample_bin2 () = sample_bin(), sample_bin()

let test_ints = [0;1;2;3;4;5;6;7;8; 123346236]
let test_bins2 =
  List.flatten
    (List.map
       (fun x -> List.map (fun y -> Solution.encode x, Solution.encode y) test_ints)
       test_ints)


let exercise =
  [
   Section (
     [ Code "is_binary" ],
       test_function_1_against_solution [%ty : liste_bool -> bool]
         ~gen:0 "is_binary" [Nothing;
                             OneMore(true, Nothing);
                             OneMore(false, Nothing);
                             OneMore(true, OneMore(false, Nothing));
                             OneMore(true, OneMore(true, Nothing));
                             OneMore(true, OneMore(true, OneMore(true, Nothing)));
                            ]
   );
   Section (
     [ Code "encode" ], test_function_1_against_solution [%ty : int -> liste_bool]
      ~gen:10 ~sampler:sample_pos "encode" test_ints
   );
   Section (
     [ Code "decode" ], test_function_1_against_solution [%ty : liste_bool -> int] "decode"
       ~gen:10 ~sampler:sample_bin (List.map Solution.encode test_ints)
   );
   Section (
     [ Code "plus_bin" ], test_function_2_against_solution [%ty : liste_bool -> liste_bool -> liste_bool]
       "plus_bin" ~gen:10 ~sampler:sample_bin2 test_bins2
   )
]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

