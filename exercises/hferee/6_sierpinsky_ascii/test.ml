
open Test_lib
open Report

let sample_int () = abs (sample_int ())

let all_char i j = String.make 1 (Char.chr (i + 16 * j))


let sample_s() =
  let n = sample_int() in
  let (h, w) = Solution.size_sierpinsky n in
  let h = sample_int() mod  h
  and w = sample_int() mod w in
  (n, h, w)


let ints = [0; 1; 2; 3; 4; 5]
let exercise =
  [
    Section(
      [Code "size_sierpinsky"],
      test_function_1_against_solution [%ty: int -> int * int]
        "size_sierpinsky" ~gen:0 ints
    );
    Section(
      [Code "s"],
      test_function_3_against_solution [%ty: int -> int -> int -> string]
        "s" ~gen:30 ~sampler:sample_s []
    );
    Section(
      [Code "draw"],
      test_function_2_against_solution
        [%ty: (int -> int -> string) -> (int * int) -> unit] "draw"
        ~test:test_ignore ~gen:0 ~test_stdout:io_test_lines
        [(all_char, (16, 16))]
    );
    Section(
      [Code "sierpinsky"],
      test_function_1_against_solution [%ty: int -> unit]
        "sierpinsky" ~test:test_ignore ~gen:0
        ~test_stdout:io_test_lines ints
    );

  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

