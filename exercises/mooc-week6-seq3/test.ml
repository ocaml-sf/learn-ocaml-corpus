open Report
open Test_lib

let sample_char =
  let last = ref 'x' in
  fun () ->
    if Random.int 4 > 0 then last := Char.chr (Random.int 26 + Char.code 'a') ;
    !last

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "CharHashedType" ],
           test_variable_property
             [%ty: (module Hashtbl.HashedType with type t = char) ] "CharHashedType"
             (fun _ ->
                [ Message ([ Text "Bravo!" ], Success 5) ]) @
           test_function_2_against_solution ~gen: 10
             [%ty: char -> char -> bool] "CharHashedType.equal"
             [ (* auto gen *) ])

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "CharHashtbl" ],
           test_variable_property
             [%ty: (module Hashtbl.S with type key = char) ] "CharHashtbl"
             (fun _ ->
                [ Message ([ Text "Bravo!" ], Success 5) ]))

let sample_key =
  sample_cases
    [ "roberto" ; "yann" ; "ralf" ;
      "benjamin" ; "gregoire" ; "cagdas" ]

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "Trie" ],
           test_variable_property
             [%ty: (module Hashtbl.S with type key = char) ] "CharHashtbl" @@
           fun (module CharHashtbl) ->
           compatible_type
             ~expected:"Code.CharHashtbl.t"
             "Trie.char_table" @
           test_variable_property
             [%ty: (module GenericTrie) ] "Trie" @@
           fun (module Trie) ->
           [ Message ([ Text "Bravo!" ], Success 5) ] @
           let rec test nb =
             if nb = 0 then
               []
             else
               let key =
                 sample_key () in
               let seq =
                 (sample_key (), sample_int ()) ::
                 sample_list (fun () -> sample_key (), sample_int ()) () in
               let msg =
                 Message
                   ([ Text "Trying to insert" ;
                      Code (String.concat "\n"
                              (List.map (fun (n, v) -> Printf.sprintf " - %S -> %d" n v) seq)) ;
                      Break ;
                      Text "And then lookup " ;
                      Code (Printf.sprintf " %S" key) ;
                      Break ], Informative) in
               let exp = try
                   let set =
                     List.fold_left
                       (fun acc (n, v) -> Solution.Trie.insert acc n v)
                       (Solution.Trie.empty ()) seq in
                   Ok (Solution.Trie.lookup set key) with exn -> Error exn in
               let got = try
                   let set =
                     List.fold_left
                       (fun acc (n, v) -> Trie.insert acc n v)
                       (Trie.empty ()) seq in
                   Ok (Trie.lookup set key) with exn -> Error exn in
               (msg :: test_eq (=) [%ty: int option] got exp) :: test (nb - 1) in
           test 10 |> List.flatten)


let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ]
