open Report
open Test_lib

let corpus =
   [| "alpha" ; "bowl" ; "clown" ; "diddy" ; "elephant" |]

let sample_string () =
  Array.unsafe_get corpus (Random.int (Array.length corpus))

let rec destutter = function
  | ([] | [_]) as l -> l
  | x :: (y :: _ as l) when x = y -> destutter l
  | x :: l -> x :: destutter l

let destutter arr = Array.of_list (destutter (Array.to_list arr))

let is_sorted a =
  let rec aux k =
    if k + 1 < Array.length a then
      if String.compare a.(k) a.(k + 1) < 0 then
	aux (succ k)
      else
	false
    else
      true
  in
  aux 0

let sorted_array sample () =
  let arr = Array.init (Random.int 3 + 4) (fun _ -> sample ()) in
  Array.sort compare arr; destutter arr

let rec unsorted_array sample () =
  let arr = Array.init (Random.int 3 + 4) (fun _ -> sample ()) in
  if is_sorted arr then unsorted_array sample () else arr

let sample_array sample =
  sample_alternatively
    [ sorted_array sample ;
      unsorted_array sample ]

let exercise_1 =
  Section ([ Text "Exercise 1:" ; Code "is_sorted " ],
           test_function_1_against_solution
             [%ty: string array -> bool] "is_sorted"
             [ [| |] ; [| "single" |]])

let corpus =
  [| "a" ; "b" ; "c" ; "d" ; "e" ; "f" ; "g" ;
     "h" ; "i" ; "j" ; "k" ; "l" ; "m" ; "n" |]

let sample_string () =
  Array.unsafe_get corpus (Random.int (Array.length corpus))

type string_array = string array

let sample_string_array () =
  let ofs = Random.int 5 in
  Array.sub corpus ofs (Random.int (Array.length corpus - ofs))

let exercise_2 =
  Section ([ Text "Exercise 2:" ; Code "find " ],
           test_function_2_against_solution
             ~before_user: (fun _ _ -> Array.counter := 0)
             ~after: (fun arr _ _ _ ->
                 let ops = !Array.counter in
                 let max_ops = int_of_float (log (float (Array.length arr)) /. log 2.) in
                 let msg = Format.asprintf "Search in %d operations." ops in
                 Report.[ Message ([ Text msg ],
                                   if ops > max_ops + 2 then Failure else Success 1) ])
             [%ty: string_array -> string -> int] "find"
             [ [| "a" ; "b" ; "c" |], "a" ;
               [| "a" ; "b" ; "c" |], "b" ;
               [| "a" ; "b" ; "c" |], "c" ;
               [| "a" ; "b" ; "c" |], "d" ;
               [| |], "" ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ]
