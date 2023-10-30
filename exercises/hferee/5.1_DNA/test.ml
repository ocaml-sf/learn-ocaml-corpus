
open Test_lib
open Report

let sample_base () =
  match Random.int 4 with
  | 0 -> "A"
  | 1 -> "C"
  | 2 -> "G"
  | _ -> "T"

let sample_dna () =
  let rec aux n = if n <= 0 then "" else
    sample_base() ^ aux (n - 1) in
  aux (Random.int 50)

let test_dna = [""; "A"; "C"; "G"; "T"; "ACGT"; "a"; "$"; "ACG$T"; "TAA";
                "ATAA"; "AZETAATAAT"]
let test_dna' = [""; "A"; "C"; "G"; "T"; "ACGT"; "TAA"; "ATAA";]


let exercise =
  [
    Section(
      [Code "is_dna"],
      test_function_1_against_solution [%ty: string -> bool] ~gen:20 "is_dna"
        test_dna
    );
    Section(
      [Code "complement"],
      test_function_1_against_solution [%ty: string -> string] ~gen:20
        ~sampler:sample_dna
        "complement" test_dna'
    );
    Section(
      [Code "first_stop"],
      test_function_1_against_solution [%ty: string -> int] ~gen:20 "first_stop"
        ~sampler:sample_dna
        test_dna'
    );
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@
  fun () ->
  exercise

