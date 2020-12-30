open Report
open Test_lib

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "wrap" ],
           test_function_1_against_solution ~gen:5
             [%ty: int list-> int list list] "wrap"
             [] @
           test_function_1_against_solution ~gen:5
             [%ty: string list -> string list list] "wrap"
             [])

let sample_tree sampler () =
  let node_nb = Random.int 10 in
  let rec gen_tree nb =
    if nb > 0 then
      let dir = Random.int 9 in
      if dir < 3 then
        let left, rem = gen_tree (nb-1) in
        let right, rem = gen_tree rem in
        Node (left, sampler (), right), rem
      else if dir < 6 then
        let right, rem = gen_tree (nb-1) in
        let left, rem = gen_tree rem in
        Node (left, sampler (), right), rem
      else
        Leaf (sampler ()), nb
    else
      Leaf (sampler ()), nb
  in
  gen_tree node_nb |> fst

type int_int = int -> int
let sample_int_int =
  sample_cases
    [ ( * ) 2; (-) 3; (+) 42 ]

type int_bool = int -> bool
let sample_int_bool =
  sample_cases
    [ (fun x -> x mod 2 = 0); (=) 42; (<>) 3 ]

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "tree_map" ],
           test_function_2_against_solution
             [%ty: int_int -> int tree -> int tree ] "tree_map"
             [] @
           test_function_2_against_solution
             [%ty: int_bool -> int tree -> bool tree ] "tree_map"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ]
