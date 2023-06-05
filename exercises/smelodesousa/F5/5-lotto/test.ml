open Test_lib
open Report

exception FoundUnique of int

let rec remove matches = function
  | [] -> []
  | x :: xs when x = matches -> remove matches xs
  | x :: xs -> x :: remove matches xs

let init n f =
  let l = ref [] in
  for i = 0 to n-1 do
    l := (f i)::!l
  done;
  !l

(* n: maximum size of generated list *)
let generate_list () =
  let possible_values, possible_values_sz = (ref (init 49 (fun x -> x+1))), ref 49 in
  let sz = (Random.int 25) + 5 in
  let l = ref [] in
  for i = 0 to (sz-1) do
    (* there cant be any duplicates in the list *)
    let v = List.nth !possible_values (Random.int !possible_values_sz) in
    possible_values := remove v !possible_values;
    possible_values_sz := !possible_values_sz - 1;
    l := v::(!l)
  done;
  !l

let generate_grid l =
  let g = Array.make_matrix 7 7 false in
  
  List.iter (fun e -> 
    let e = if (Random.int 100) >= 80 then (Random.int 49) else e-1 in
    let i = (e/7) in
    let j = abs(e - (i*7)) in
    begin 
      match (Random.int 100) with
      | _ as x when x >= 0 && x <= 80 -> g.(i).(j) <- false
      | _ -> g.(i).(j) <- true
    end;
   )l;
  g

let draw_sampler () = 
  let l = generate_list () in
  let g = generate_grid l in
  let complementary = 
    (try while true do
        let v = (Random.int 49) + 1 in
        if not (List.mem v l) then raise (FoundUnique v)
      done;
      -1
    with FoundUnique v -> v) in
  g, l, complementary

let correct_answer id name =
 Section ([ Text id ; Code "solution" ],
  [Message ([ Text "Checking that " ; Code name ; Text "is correct "], Informative) ;
    Message ([ Text "Correct answer" ],  Success 5)])

let wrong_answer id name =
 Section ([ Text id ; Code "solution" ],
  [Message ([ Text "Checking that " ; Code name ; Text "is correct "], Informative) ;
    Message ([ Text "Wrong answer" ],  Failure)])

let compatible_type ~expected:exp got =
  match Introspection.compatible_type exp ("Code." ^ got) with
  | Introspection.Absent     -> false
  | Introspection.Incompatible _ -> false
  | Introspection.Present () -> true

let q1 = 
  Section([Text "Testing make_grid function"],
          test_function_1_against_solution
          [%ty: int -> bool array array] 
          "make_grid"
          ~gen:8
          ~sampler: (fun () -> Random.int 20)
          [0; 10]
    )

type correct_p1 = bool array array
let q2 = 
  let r2 = compatible_type "correct_p1" "grids" in
  match r2 with
  | true -> correct_answer "Exercise 2: " "grids"
  | _  -> wrong_answer "Exercise 2: " "grids"

let q3 =
  let grid = Array.make_matrix 7 7 false in
  Section([Text "Testing global variable grid"],
      test_variable_against_solution
      [%ty: bool array array]
      "grid"
  )

let q4 = 
  Section([Text "Testing fill function"],
          test_function_1_against_solution
          [%ty: int list -> bool array array] 
          "fill"
          ~gen:17
          ~sampler: generate_list
          [[]; [1; 49]; [5; 6; 25; 10]]
    )

let q5 = 
  Section([Text "Testing prize_draw function"],
          test_function_3_against_solution
          [%ty: bool array array -> int list -> int -> (int list * bool)] 
          "prize_draw"
          ~gen:40
          ~sampler:draw_sampler
          []
    )

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [q1; q2; q3; q4; q5]
