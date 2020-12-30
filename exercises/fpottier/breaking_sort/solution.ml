(* This solution uses a combination of random testing and exhaustive testing.
   We first test all list lengths from 0 to 6, picking random elements of
   varying amplitude, and test all permutations of these random elements.
   (Don't ask why we do this; it is somewhat arbitrary.) Then, we test larger
   lengths, picking random elements. *)

(* This solution does not achieve the maximal score, but comes close to it. *)

(* This solution performs no shrinking. *)

exception FoundTestInput of int list

let test1 sort xs =
  let zs = List.sort compare xs in
  match sort xs with
  | ys ->
      if ys <> zs then
        raise (FoundTestInput xs)
  | exception _ ->
      raise (FoundTestInput xs)

let test sort =
  (* Work by increasing lengths. *)
  for n = 0 to 6 do
    [ 2; 4; 8; 128; 2048 ] |> List.iter (fun amplitude ->
      for k = 0 to 4 do
        (* At each length [n], randomly pick [n] integer elements in an array. *)
        let xs = Array.init n (fun i -> Random.int amplitude - amplitude / 2) in
        (* Convert into a list, and test the algorithm. *)
        let xs = Array.to_list xs in
        test1 sort xs
      done
    )
  done;
  (* Work by increasing lengths, again, but less thoroughly. *)
  for n = 7 to 512 do
    for k = 0 to 10 do
      (* At each length [n], randomly pick [n] integer elements in an array. *)
      let xs = init n (fun i -> Random.int 2048 - 1024) in
      test1 sort xs
    done
  done

let test sort =
  try
    test sort;
    None
  with FoundTestInput xs ->
    Some xs

let inputs : int list option list =
  List.map test sorts
