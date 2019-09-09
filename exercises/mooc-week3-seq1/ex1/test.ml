open Report
open Test_lib

let sample_queue () =
  let l1 = sample_list sample_int () in
  let l2 = sample_list sample_int () in
  (l1, l2)

let exercise_1 =
  Section ([ Text "Exercise 1: " ; Code "is_empty" ],
           test_function_1_against_solution
             [%ty: queue -> bool] "is_empty"
             [ ([],[]); ([],[1]); ([2],[]) ])

let cannon_queue ((front, back):queue) : queue =
  front @ List.rev back, []

let exercise_2 =
  Section ([ Text "Exercise 2: " ; Code "enqueue" ],
           test_function_2_against_solution
             ~test:(test_canon_ok cannon_queue)
             [%ty: int -> queue -> queue] "enqueue"
             [])

let cannon_split ((front, back):queue) : queue =
  back @ List.rev front, []

let after input ((front, back), _, _) (_, _, _) =
  let l = List.length input in
  let l1 = l / 2 in
  let l2 = l / 2 + 1 in
  let l_front = List.length front in
  let l_back = List.length back in
  let l_front_ok = l_front = l1 || l_front = l2 in
  let l_back_ok = l_back = l1 || l_back = l2 in
  let msg name len =
    if len = l1 || len = l2 then
      Format.asprintf "%s length is %d." name len
    else
      Format.asprintf "%s length is %d, it should be %d or %d." name len l1 l2
  in
  Report.[ Message ([ Text (msg "Front" l_front);
                      Text (msg "Back" l_back); ],
                    if l_front_ok && l_back_ok then Success 1 else Failure) ]

let exercise_3 =
  Section ([ Text "Exercise 3: " ; Code "split" ],
           test_function_1_against_solution
             ~test:(test_canon_ok cannon_split)
             ~after
             [%ty: int list -> queue] "split"
             [])

let rec sample_queue () =
  let l1 = sample_list sample_int () in
  let l2 = sample_list sample_int () in
  if l1 = [] && l2 = [] then
    sample_queue ()
  else
    (l1, l2)

let cannon_int_queue (x, (front, back)) =
  x, (front @ List.rev back, [])

let exercise_4 =
  Section ([ Text "Exercise 4: " ; Code "dequeue" ],
           test_function_1_against_solution
             ~test:(test_canon_ok cannon_int_queue)
             [%ty: queue -> int * queue] "dequeue"
             [([],[3;2;1])])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ]
