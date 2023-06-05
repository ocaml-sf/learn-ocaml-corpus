
open Test_lib
open Report

let exercise =
  [
    Section(
      [Code "range"],
      test_function_1_against_solution [%ty : int -> liste] ~gen:0 "range" [0;
                                                                            1;
                                                                            2;
                                                                            8]
    );
    Section(
      [Code "filter"],
      test_function_2_against_solution [%ty : (int -> bool) -> liste -> liste]
        "filter" ~gen:0 [(fun x -> x mod 2 <> 0), Nothing; (fun x -> x mod 2 <>
                                                                   0), Solution.range 15]
    );
    Section(
      [Code "sieve"],
      test_function_1_against_solution [%ty: int -> liste] "sieve" ~gen:0 [0; 1; 2; 5;
                                                                   100]
  )
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

