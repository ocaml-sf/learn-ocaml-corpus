open Test_lib
open Report

(* n: maximum size of generated list *)
let generate_list n =
  let sz = Random.int n + 1 in
  let l = ref [] in
  for i = 0 to sz - 1 do
    l := (Random.int (2 * 9999) - 9999) :: !l
  done;
  (!l, sz)

(* generates a sublist of l, of size sz, with a maximum size of n *)
let rec generate_sublist l =
  let pick_number () = Random.int 100 > 40 in
  match l with
  | [] -> []
  | h :: t ->
      if pick_number () then h :: generate_sublist t else generate_sublist t

let sampler_subseq () =
  match Random.int 100 with
  | _ as x when x >= 30 ->
      let l1, sz = generate_list 25 in
      let l2 = generate_sublist l1 in
      (l2, l1)
  | _ ->
      let l1, _ = generate_list 25 in
      let l2, _ = generate_list 12 in
      (l2, l1)

let test_with_solution =
  set_progress "Grading exercise";
  Section
    ( [ Text "Testing function"; Code "subseq" ],
      test_function_2_against_solution [%ty: int list -> int list -> bool]
        "subseq" ~gen:19 ~sampler:sampler_subseq
        [ ([], []); ([ 4; 7; 5; 1 ], [ 4; 5; 4; 6; 2; 7; 5; 6; 8; 1; 0 ]) ] )

let () =
  set_result @@ ast_sanity_check code_ast @@ fun () -> [ test_with_solution ]
