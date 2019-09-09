open Report
open Test_lib

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "MultiSet" ],
           test_variable_property
             [%ty: (module MultiSet_S)] "MultiSet" @@ fun (module MultiSet : MultiSet_S) ->
           [ Message ([ Text "Your module is compatible with the" ;
                        Code "MultiSet" ; Text "signature." ], Success 5)])

module type Code_S = sig
  module MultiSet : MultiSet_S
end

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "letters" ],
           (* This is an ugly hack, [Code] shadows the global [Code],
              it is the same module, under a different, included signature. *)
           test_student_code [%ty: (module Code_S)] @@ fun (module Code : Code_S) ->
           let letters word =
             let s = ref Code.MultiSet.empty in
             String.iter (fun c -> s := Code.MultiSet.insert !s c) word;
             !s in
           let to_assoc ms =
             let res = ref [] in
             for i = 0 to 255 do
               res := (Char.chr i, Code.MultiSet.occurrences ms (Char.chr i)) :: !res
             done ; List.filter (fun (_, n) -> n <> 0) (List.rev !res) in
           let test = test_translate to_assoc test [%ty: (char * int) list] in
           test_function_1_against ~test
             [%ty: string -> char Code.MultiSet.t] "letters" letters [])

let permut s =
  let s = String.copy s in
  for i = String.length s - 1 downto 2 do
    let j = Random.int (i+1) in
    let x = s.[i] in
    s.[i] <- s.[j] ;
    s.[j] <- x
  done ;
  s

let exercise_3 =
  set_progress "Grading exercise 3." ;
  let sampler =
    let false_ () =
      (sample_string (), sample_string ())
    and true_ () =
      let s = sample_string () in
      (s, permut s) in
    sample_alternatively  [ false_ ; true_ ]
  in
  Section ([ Text "Exercise 3: " ; Code "anagram" ],
           test_function_2_against_solution
             ~sampler [%ty: string -> string -> bool] "anagram" [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ]
