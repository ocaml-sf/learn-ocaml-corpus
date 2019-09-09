open Report
open Test_lib

let sample_char () =
  Char.chr (Random.int 7 * 3 + Char.code 'a')

let sample_trie, sample_string =
  let sample_word () =
    String.init (Random.int 2 + 2) (fun i -> sample_char ()) in
  let prefix s =
    String.sub s 0 (Random.int (String.length s - 1)) in
  let of_list l =
    List.fold_left (fun trie w -> Solution.insert trie w (sample_int ())) empty l in
  let domain = ref [||] in
  let sample_trie () =
    domain := sample_array ~min_size:2 ~max_size: 6 ~dups: false sample_word () ;
    domain := Array.concat [ [| prefix (!domain).(0) ; prefix (!domain).(1) |] ; !domain ] ;
    of_list (Array.to_list !domain) in
  let sample_string =
    sample_alternatively
      [ (fun () -> (!domain).(Random.int (Array.length !domain))) ;
        sample_word ] in
  sample_trie, sample_string

let sample_char_to_children () =
  let Trie (_, res) = sample_trie () in
  res

let rec canon_char_to_children l =
  let l = List.map (fun (c, t) -> (c, canon_trie t)) l in
  List.sort (fun (a, _) (b, _) -> compare a b) l
and canon_trie (Trie (c, l)) =
  Trie (c, canon_char_to_children l)

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "children_from_char" ],
           test_function_2_against_solution
             ~test: (test_canon_ok (function None -> None | Some t -> Some (canon_trie t)))
             [%ty: char_to_children -> char -> trie option] "children_from_char"
             [])
let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "update_children" ],
           test_function_3_against_solution
             ~test: (test_canon_ok canon_char_to_children)
             [%ty: char_to_children -> char -> trie -> char_to_children] "update_children"
             [])
let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "lookup" ],
           test_function_2_against_solution
             [%ty: trie -> string -> int option] "lookup"
             [])
let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "insert" ],
           test_function_3_against_solution
             ~test: (test_canon_ok canon_trie)
             [%ty: trie -> string -> int -> trie] "insert"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ]
