(* [is_sorted (<=) a] determines whether the array [a] is sorted with respect
   to the preorder [(<=)]. *)

let is_sorted ((<=) : 'a -> 'a -> bool) (a : 'a array) : bool =
  let n = length a in
  let rec is_sorted_from i =
    i >= n-1 || a.(i) <= a.(i+1) && is_sorted_from (i+1)
  in
  is_sorted_from 0

(* [is_permutation p] tests whether the array [p] represents a valid
   permutation of the semi-open interval [0,n), where [n] is [length p]. *)

let is_permutation (p : int array) : bool =
  let exception Invalid in
  let n = length p in
  let hit = make n false in
  try
    for i = 0 to n-1 do
      if 0 <= p.(i) && p.(i) < n then
        if hit.(p.(i)) then
          (* Not injective. *)
          raise Invalid
        else
          hit.(p.(i)) <- true
      else
        (* Out of range. *)
        raise Invalid
    done;
    true
  with Invalid ->
    false

(* [leq_suffix_suffix s i j] is equivalent to [suffix s i <= suffix s j]. *)

let leq_suffix_suffix s i j =
  let n = String.length s in
  let rec loop i j =
    i = n || j < n && (
         s.[i] < s.[j]
      || s.[i] = s.[j] && loop (i+1) (j+1)
    )
  in
  i = j || loop i j

(* [is_suffix_array s a] tests whether the array [a] is the suffix array of
   the string [s]. This involves checking that [a] is a permutation of [0,n)
   and checking that [a] is lexicographically sorted. *)

let is_suffix_array (s : string) (a : suffix array) : bool =
  let n = String.length s in
  length a = n &&
  is_permutation a &&
  is_sorted (leq_suffix_suffix s) a

(* This wrapper adapts a comparison function that returns a Boolean result
   so as to obtain a comparison function that returns a three-way result,
   encoded as one of the integers [-1], [0], and [1]. *)

let cmp leq x y =
  if leq x y then
    if leq y x then
      0
    else
      -1
  else
    1

(* [naive_suffix_sort s] computes the suffix array of the string [s]. *)

let naive_suffix_sort (s : string) : suffix array =
  let n = String.length s in
  let a = init n (fun i -> i) in
  Array.sort (cmp (leq_suffix_suffix s)) a;
  a

(* [invert] inverts a permutation, represented as an array of integers.
   Thus, [invert p] returns a new array [q] such that [q.(p.(i))] is [i]
   and [p.(q.(j))] is [j]. *)

let invert (p : int array) : int array =
  let n = length p in
  let q = make n 0 in
  for i = 0 to n-1 do
    q.(p.(i)) <- i
  done;
  q

let postincrement (c : int ref) : int =
  let i = !c in
  c := i + 1;
  i

(* [pigeonhole_sort] sorts an array of values according to their key,
   where the function [key] maps a value to a key in the range [0..m).
   It is a stable sort. Sorting is performed in place. *)

let pigeonhole_sort (m : int) (key : 'v -> int) (a : 'v array) : unit =
  (* Create an array of [n] buckets, each of which holds a list of values.
     Every bucket is initially empty. *)
  let bucket : 'v list array = make m [] in
  (* Place every value into an appropriate bucket, according to its key. *)
  iter (fun v ->
    let k = key v in
    bucket.(k) <- v :: bucket.(k)
  ) a;
  (* Concatenate all buckets, writing the result to the array [a]. The
     elements in each bucket are written in reverse order, so that the
     element that was added last to the bucket is written last to the
     destination array. This guarantees a stable sort. *)
  let i = ref 0 in
  let write v = a.(postincrement i) <- v in
  let write_bucket vs = List.iter write (List.rev vs) in
  iter write_bucket bucket

(* [beginning_of_bucket] takes the same arguments as [pigeonhole_sort]
   except [m] which is not needed. It assumes that the array [a] is
   sorted already. It returns an array [b] such that [b.(i)] is [true]
   if and only if the index [i] marks the beginning of a new bucket,
   that is, the element [a.(i)] does not have the same key as its
   predecessor. *)

let beginning_of_bucket (key : 'v -> int) (a : 'v array) : bool array =
  let n = length a in
  begin
    (* Initialize the array [b] to [true] everywhere. At index 0, this is the
       correct value, which is why the following loop begins at [1]. *)
    let b = make n true in
    (* In a single pass over the array [a], find every element that does not
       represent the beginning of a new bucket, i.e., every element that has
       the same key as its predecessor, and set its [b] entry to [false]. *)
    let predecessor_key = ref (key a.(0)) in
    for i = 1 to n-1 do
      let k = key a.(i) in
      if !predecessor_key = k then b.(i) <- false;
      predecessor_key := k
    done;
    b
  end

let previous (b : bool array) : int array =
  let n = length b in
  let p = make n 0 in
  let last = ref 0 in
  for i = 1 to n-1 do
    if b.(i) then last := i;
    p.(i) <- !last
  done;
  p

let buckets (b : bool array) : (int * int) list =
  let bs = ref [] in
  let n = length b in
  let l = ref 0 in
  for i = 1 to n do
    if i = n || b.(i) then begin
      bs := (!l, i) :: !bs;
      l := i
    end
  done;
  List.rev !bs

let population (b : bool array) : int =
  let p = ref 0 in
  let n = length b in
  for i = 0 to n-1 do
    if b.(i) then p := !p + 1
  done;
  !p

(* [m] is the size of the alphabet. *)

let m =
  256

type index =
  int

(* The array [a] should be viewed as a function of an index to a suffix.
   E.g., [a.(c)] tells us what suffix currently appears at position [c]
   in the array.
   The array [rank] is a function of a suffix to an index.
   E.g., [rank.(i)] tells us at what position in the array the suffix [i]
   appears. *)

type allocator = {
  (* The allocation limit in bucket [e]. *)
  count: int array;
  (* The first location allocated in bucket [e] during this round. *)
  first: int option array;
  (* A list of all buckets in which at least one allocation took place
     within this round. This list does not contain duplicate elements. *)
  touched: int list ref;
}

let allocator (n : int) : allocator =
  let count = make n 0 in
  let first = make n None in
  let touched = ref [] in
  { count; first; touched }

let next { count; first; touched } (e : index) : index =
  let location = e + count.(e) in
  count.(e) <- count.(e) + 1;
  if first.(e) = None then begin
    first.(e) <- Some location;
    touched := e :: !touched
  end;
  location

let end_round { count; first; touched } (action : index -> unit) =
  !touched |> List.iter (fun e ->
    match first.(e) with
    | Some location ->
        action location;
        first.(e) <- None
    | None ->
        assert false
  );
  touched := []
  (* [count] is not reset. Allocation continues in the next round. *)

let rec stage (h : int) (s : string) (a : suffix array) (b : bool array) (rank : int array) =
  let n = String.length s in
  if h >= n || population b = n then
    (* If I am not mistaken, either of the two conditions above alone would
       be sufficient to ensure termination. The condition [h >= n] implies
       the condition [population b = n], because no two suffixes are equal. *)
    a
  else begin
    (* For every suffix [d], [start d] is the index of the beginning
       of the bucket where [d] lies. *)
    let p = previous b in
    let start d = p.(rank.(d)) in
    (* Move. *)
    let allocator = allocator n in
    let move (d : suffix) =
      (* [e] is the [h]-bucket where [d] lies. *)
      (* We allocate a new slot from this bucket
         and move [d] into it by updating [rank]. *)
      let e : index = start d in
      let dst = next allocator e in
      rank.(d) <- dst
    in
    (* Prepare to mark the beginning of each [2h]-bucket. *)
    let b' = copy b in
    let mark dst = b'.(dst) <- true in
    (* The following loop is injective: [move d] is called at most once
       per suffix [d]. *)
    move (n - h);
    end_round allocator mark; (* in this case, just one location, and it is already marked *)
    buckets b |> List.iter (fun (l, r) ->
      for c = l to r-1 do
        let d = a.(c) - h in
        if 0 <= d then
          move d
      done;
      end_round allocator mark
    );
    (* Invert. *)
    let a = invert rank in
    stage (2*h) s a b' rank
  end

let suffix_sort (s : string) =
  (* Stage 0. Create an array [a] of all suffixes. Sort this array, based
     on the first character of each suffix. Create an array [b] indicating
     where each bucket begins. *)
  let n = String.length s in
  let a : suffix array = init n (fun i -> i) in
  let key i = Char.code s.[i] in
  pigeonhole_sort m key a;
  let b = beginning_of_bucket key a in
  let rank = invert a in
  (* Now move on to stages 1, 2, 4, 8, etc. *)
  stage 1 s a b rank
