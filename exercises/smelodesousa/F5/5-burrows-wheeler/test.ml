open Test_lib
open Report
open List
open Solution

let rec generate_word n =
  String.concat ""
    (map
       (fun _ -> String.make 1 (char_of_int (97 + Random.int 26)))
       (init n (fun _ -> ())))

let bwtT =
  Section
    ( [ Text "Testing function"; Code "bwt" ],
      test_function_1_against_solution [%ty: string -> int * string] "bwt"
        ~sampler:(fun () -> generate_word (Random.int 4 + 5))
        ~gen:10
        [ "anagrama"; "abraca"; "yokohama"; "tototo"; "mosaissova" ] )

let debwtT =
  Section
    ( [ Text "Testing function"; Code "debwt" ],
      test_function_1_against_solution [%ty: int * string -> string] "debwt"
        ~sampler:(fun () -> bwt (generate_word (Random.int 4 + 5)))
        ~gen:10
        [
          (2, "caraab");
          (8, "hmooakya");
          (4, "tttooo");
          (4, "svaamsosio");
          (4, "mnraaaag");
        ] )

let () = set_result @@ ast_sanity_check code_ast @@ fun () -> [ bwtT; debwtT ]