open Printf
(* A correct sort. *)
let sort = List.sort Stdlib.compare

exception TODO

(* -------------------------------------------------------------------------- *)

(* We need everything to be deterministic. *)

let () = Random.init 0

(* -------------------------------------------------------------------------- *)

(* Utilities. *)

let init n f =
  Array.init n f |> Array.to_list

let rec is_power_of_two n =
  match n with
  | 0 -> false
  | 1 -> true
  | _ ->
      n mod 2 = 0 && is_power_of_two (n / 2)

let enum_powers_of_two n k =
  let p = ref 1 in
  k !p;
  for i = 1 to n do
    p := 2 * !p;
    (* !p = 2^i *)
    k !p
  done

let rec cut n xs =
  match n, xs with
  | 0, _
  | _, [] ->
      [], xs
  | _, x :: xs ->
      let xs, ys = cut (n - 1) xs in
      x :: xs, ys

let tail xs =
  match xs with
  | [] ->
      []
  | x :: xs ->
      xs

let rec no_duplicates xs =
  match xs with
  | []
  | _ :: [] ->
      xs
  | x0 :: x1 :: xs ->
      if x0 = x1 then
        no_duplicates (x1 :: xs)
      else
        x0 :: no_duplicates (x1 :: xs)

let rec no_triplicates xs =
  match xs with
  | []
  | _ :: []
  | _ :: _ :: [] ->
      xs
  | x0 :: x1 :: x2 :: xs ->
      if x0 = x1 && x1 = x2 then
        no_triplicates (x1 :: x2 :: xs)
      else
        x0 :: no_triplicates (x1 :: x2 :: xs)

let if_length (p : int -> bool) sort1 sort2 =
  fun xs ->
    if p (List.length xs) then sort1 xs
    else sort2 xs

let if_length_reaches k sort1 sort2 =
  if_length (fun n -> n >= k) sort1 sort2

let swap xs i j =
  if i <> j then
    let x = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- x

let shuffle (xs : 'a array) : unit =
  let n = Array.length xs in
  for i = n downto 2 do
    swap xs (i-1) (Random.int i)
  done

let permutations (xs : 'a array) (k : unit -> unit) : unit =
  let n = Array.length xs in
  let rec loop i =
    if i <= 1 then
      k()
    else
      let i = i - 1 in
      for j = 0 to i do
        swap xs i j;
        loop i;
        swap xs i j
      done
  in
  loop n

(* -------------------------------------------------------------------------- *)

(* A bunch of flawed [sort] functions. *)

type candidate =
  string * int * (int list -> int list)
    (* internal name, size of minimal problematic input, sort function *)

let candidates : candidate list ref =
  ref []

let register candidate  =
  candidates := candidate :: !candidates

(* Raises an exception. *)

let fail xs =
  raise Not_found

let () =
  for k = 0 to 5 do
    register (sprintf "fail%d" k, k, if_length_reaches k fail sort)
  done

(* Returns an empty list. *)

let empty xs =
  []

let () =
  for k = 1 to 5 do
    register (sprintf "empty%d" k, k, if_length_reaches k empty sort)
  done

(* Returns a constant list. *)

let constant ks xs =
  ks

let () =
  List.iter (fun ks ->
    for k = 0 to 5 do
      register (
        sprintf "constant%d" k,
        k,
        if_length_reaches k (constant ks) sort
      )
    done
  ) [
    [1;2;3;4];
    [1;1;1;1]
  ]

(* Returns a sorted list of appropriate length. *)

let mimic xs =
  init (List.length xs) (fun i -> 0)

let () =
  register ("mimic", 1, mimic)

(* Returns a list that is sorted in reverse order. *)

let backwards xs =
  List.rev (sort xs)

let () =
  register ("backwards", 2, backwards)

(* Returns the input list. *)

let identity xs =
  xs

let () =
  register ("identity", 2, identity)

(* Fails if the list is empty. *)

let () =
  register ("fail_if_empty", 0, if_length (fun n -> n = 0) fail sort)

(* Fails if the list length is a power of two. *)

let () =
  register ("fail_of_power_of_two", 1, if_length is_power_of_two fail sort)

(* Fails if the list length is a certain power of two. *)

let () =
  enum_powers_of_two 5 (fun s ->
    register (
      sprintf "fail_if_length_is_%d" s,
      s,
      if_length (fun n -> n = s) fail sort)
  )

(* Sorts only a subrange: all but the first element. *)

let subrange xs =
  match xs with
  | [] ->
      []
  | x :: xs ->
      x :: sort xs

let () =
  register ("subrange", 2, subrange)

(* Fails subtly if the list length is a power of two. *)

let () =
  register (
    "subrange_if_power_of_two",
    2,
    if_length is_power_of_two subrange sort
  )

(* Fails obviously if the list length exceeds a certain limit. *)

let () =
  enum_powers_of_two 8 (fun s ->
    let s = s + Random.int s in
    register (
      sprintf "fail_if_length_reaches_%d" s,
      s,
      if_length_reaches s fail sort
    )
  )

(* Fails a little less obviously if the list length is greater than [s]. *)

let bogus s xs =
  let xs, ys = cut s xs in
  sort xs @ sort ys

let () =
  enum_powers_of_two 8 (fun s ->
    let s = s + Random.int s in
    register (
      sprintf "bogus_if_length_reaches_%d" s,
      s + 1,
      if_length_reaches s (bogus s) sort
    )
  )

(* Sorts correctly with respect to a different order. *)

let sort42 xs =
  let compare x y =
    match x = 42, y = 42 with
    | true, true  ->  0
    | true, false -> -1
    | false, true -> +1
    | false, false -> Stdlib.compare x y
  in
  List.sort compare xs

let () =
  register ("sort42", 2, sort42)

(* Changes the first element to something else, then sort. *)

let photon xs =
  match xs with
  | [] ->
      []
  | x :: xs ->
      sort (42 :: xs)

let () =
  register ("photon", 1, photon)

(* Fails obviously if one element reaches 256. *)

let byte xs =
  if List.for_all (fun x -> x <= 256) xs then
    sort xs
  else
    raise Not_found

let () =
  register ("byte", 1, byte)

(* Fails to sort elements above 256. *)

let sort15 xs =
  let compare x y =
    match x > 256, y > 256 with
    | true, true  ->  0
    | true, false -> -1
    | false, true -> +1
    | false, false -> Stdlib.compare x y
  in
  List.sort compare xs

let () =
  register ("sort15", 2, sort15)

(* Sorts correctly, but removes duplicates. *)

let nodups xs =
  sort xs |> no_duplicates

let () =
  register ("nodups", 2, nodups)

(* Transforms the list before sorting. *)

let transform xs =
  xs |> List.map (fun x -> -x) |> sort

let () =
  register ("transform", 1, transform)

(* Transforms a few list elements before sorting. *)

let may_transform s xs =
  xs |> List.mapi (fun i x -> if i = s-1 then x + 1 else x) |> sort

let () =
  enum_powers_of_two 5 (fun s ->
    register (sprintf "may_transform_%d" s, s, may_transform s)
  )

(* Removes the first element, then sorts. *)

let remove1 xs =
  sort (tail xs)

let () =
  register ("remove1", 1, remove1)

(* Reverses the list. *)

let () =
  register ("reverse", 2, List.rev)

(* Sorts correctly, but removes triplicates. *)

let notrips xs =
  sort xs |> no_triplicates

let () =
  register ("notrips", 3, notrips)

(* Sorts correctly, but removes triplicates if the list is long enough. *)

let () =
  for k = 4 to 8 do
    register (sprintf "notrips_%d" k, k, if_length_reaches k notrips sort)
  done

(* Apply a random permutation to the list of candidates,
   so someone who reads the source code of the exercise
   cannot tell in which order they are presented. *)

let candidates =
  let candidates = Array.of_list !candidates in
  shuffle candidates;
  Array.to_list candidates

(* Construct a list of just the [sort] functions, which will be
   presented to the student. We use lists, as opposed to arrays,
   so as to prevent cheating by writing these arrays. *)

let sorts : (int list -> int list) list =
  List.map (fun (_, _, sort) -> sort) candidates
