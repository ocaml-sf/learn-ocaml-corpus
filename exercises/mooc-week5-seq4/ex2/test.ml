open Report
open Test_lib

let sample_int () =
  Random.int 10

let sample_stack_gen ?occ ?rem () =
  let len =
    (match occ with None -> Random.int 5 | Some occ -> occ)
    + (match rem with None -> Random.int 5 | Some rem -> rem) in
  let occupied =
    match occ with
    | Some occ -> occ
    | None ->
        match rem with
        | None -> if len = 0 then 0 else Random.int len
        | Some rem -> len - rem in
  let res = Array.make (len + 1) 0 in
  res.(0) <- occupied ;
  for i = 1 to occupied do res.(i) <- Random.int 10 - 5 done ;
  res

let eq_stack got exp =
  Array.length got = Array.length exp
  && got.(0) = exp.(0)
  && Array.sub got 1 got.(0) = Array.sub exp 1 exp.(0)

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "create" ],
           test_function_1_against_solution
             ~test:(test_eq_ok eq_stack)
             [%ty: int -> stack] "create"
             [])

let sample_stack =
  sample_alternatively
    [ (fun () -> sample_stack_gen ~rem: 0 ()) ;
      (fun () -> sample_stack_gen ~rem: (Random.int 3 + 1) ()) ]

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "push" ],
           let { before_reference ; before_user ; test } =
             array_arg_mutation_test_callbacks ~test:(test_eq_ok eq_stack) [%ty: stack] in
           let before_reference a _ = before_reference a in
           let before_user a _ = before_user a in
           test_function_2_against_solution
             ~before_reference ~before_user ~test
             [%ty: stack -> int -> unit] "push"
             [])

let sampler =
  sample_alternatively
    [ (fun () ->
          let arr = sample_array ~min_size: 1 ~max_size: 5 ~dups: false sample_int () in
          sample_stack_gen ~rem: (Array.length arr + Random.int 3) (), arr) ;
      (fun () ->
         let arr = sample_array ~min_size: 2 ~max_size: 5 ~dups: false sample_int () in
         sample_stack_gen ~rem: (Random.int (Array.length arr - 1)) (), arr) ]

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "append" ],
           let { before_reference ; before_user ; test } =
             array_arg_mutation_test_callbacks ~test:(test_eq_ok eq_stack) [%ty: stack] in
           let before_reference a _ = before_reference a in
           let before_user a _ = before_user a in
           test_function_2_against_solution ~sampler
             ~before_reference ~before_user ~test
             [%ty: stack -> int array -> unit] "append"
             [ sample_stack_gen ~rem: 0 (), [||] ])

let sample_stack =
  sample_alternatively
    [ (fun () -> sample_stack_gen ~occ: 0 ()) ;
      (fun () -> sample_stack_gen ~occ: (Random.int 3 + 1) ()) ]

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "pop" ],
           let { before_reference ; before_user ; test } =
             array_arg_mutation_test_callbacks ~test:(test_eq_ok eq_stack) [%ty: stack] in
           test_function_1_against_solution
             ~before_reference ~before_user ~test:(test ~test_result:Test_lib.test)
             [%ty: stack -> int] "pop"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ]
