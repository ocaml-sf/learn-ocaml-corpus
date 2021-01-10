(* [is_sorted (<=) a] determines whether the array [a] is sorted with respect
   to the preorder [(<=)]. *)

let is_sorted ((<=) : 'a -> 'a -> bool) (a : 'a array) : bool =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  let n = length a in
  let rec is_sorted_from i =
    i >= n-1 || a.(i) <= a.(i+1) && is_sorted_from (i+1)
  in
  is_sorted_from 0
     END EXCLUDE *)

(* [is_permutation p] tests whether the array [p] represents a valid
   permutation of the semi-open interval [0,n), where [n] is [length p]. *)

let is_permutation (p : int array) : bool =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
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
     END EXCLUDE *)

(* [leq_suffix_suffix s i j] is equivalent to [suffix s i <= suffix s j]. *)

let leq_suffix_suffix s i j =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  let n = String.length s in
  let rec loop i j =
    i = n || j < n && (
         s.[i] < s.[j]
      || s.[i] = s.[j] && loop (i+1) (j+1)
    )
  in
  i = j || loop i j
     END EXCLUDE *)

(* [is_suffix_array s a] tests whether the array [a] is the suffix array of
   the string [s]. This involves checking that [a] is a permutation of [0,n)
   and checking that [a] is lexicographically sorted. *)

let is_suffix_array (s : string) (a : suffix array) : bool =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
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
     END EXCLUDE *)

(* [naive_suffix_sort s] computes the suffix array of the string [s]. *)

let naive_suffix_sort (s : string) : suffix array =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  let n = String.length s in
  let a = init n (fun i -> i) in
  Array.sort (cmp (leq_suffix_suffix s)) a;
  a

     END EXCLUDE *)

(* [postincrement c] returns the current value of the reference [c]
   and (as a side effect) increments this reference. It corresponds
   to the expression [c++] in C or Java. *)

let postincrement (c : int ref) : int =
  let i = !c in
  c := i + 1;
  i

(* [pigeonhole_sort] sorts an array of values according to their key,
   where the function [key] maps a value to a key in the range [0..m).
   It is a stable sort. Sorting is performed in place. *)

let pigeonhole_sort (m : int) (key : 'v -> int) (a : 'v array) : unit =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  (* Create an array of [m] buckets, each of which holds a list of values.
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
     END EXCLUDE *)

(* [beginning_of_bucket] takes the same arguments as [pigeonhole_sort] except
   [m] which is not needed. It assumes that the array [a] is sorted by key
   already. It returns an array [b] such that [b.(i)] is [true] if and only if
   the index [i] marks the beginning of a new bucket, that is, the element
   [a.(i)] does not have the same key as its predecessor. Keys need not be
   integers; the type of keys is an arbitrary type for which equality [=] is
   meaningful. *)

let beginning_of_bucket (key : 'v -> 'key) (a : 'v array) : bool array =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  let n = length a in
  if n = 0 then
    [||]
  else begin
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
     END EXCLUDE *)

(* -------------------------------------------------------------------------- *)

(* In order to attack the following question, a number of auxiliary functions
   are given to you. Each of them runs in linear time. *)

(* [invert] inverts a permutation [p] of the semi-open interval [0, n).
   Thus, [invert p] returns a new array [q] such that [q.(p.(i))] is [i]
   and [p.(q.(j))] is [j]. *)

let invert (p : int array) : int array =
  let n = length p in
  let q = make n 0 in
  for i = 0 to n-1 do
    q.(p.(i)) <- i
  done;
  q

(* If [b] is an array of Booleans, then the array [nearest_set_bit_left b]
   maps an index [i] to the index of the nearest [true] entry in the array
   [b], starting from position [i] and scanning towards the left. By
   convention, the index 0 is mapped to 0. Thus, if the array [b] contains
   beginning-of-bucket marks, then the array [nearest_set_bit_left b] maps an
   index [i] to the index of the beginning of the bucket that contains [i]. *)

let nearest_set_bit_left (b : bool array) : index array =
  let n = length b in
  let p = make n 0 in
  let last = ref 0 in
  for i = 1 to n-1 do
    if b.(i) then last := i;
    p.(i) <- !last
  done;
  p

(* [population b] is the number of [true] entries in the Boolean array [b].
   If the array [b] contains beginning-of-bucket marks, then [population b]
   is the number of buckets. *)

let population (b : bool array) : int =
  let p = ref 0 in
  let n = length b in
  for i = 0 to n-1 do
    if b.(i) then p := !p + 1
  done;
  !p

(* Suppose the array [a] is logically partitioned into a number of buckets and
   the array [b] contains the beginning-of-bucket marks. Then, [buckets a b]
   is a list of all buckets, where each bucket is represented as an array of
   elements.

   By convention, this list begins with an additional fictional bucket, which
   consists of just one element, namely the integer [n]. When interpreted as
   a suffix, this integer represents the empty suffix. Thus, since the array
   [a] contains all nonempty suffixes, and since we add the empty suffix, the
   list of buckets that we return covers all suffixes, that is, all of the
   integers in the closed interval [0, n].

   The data in the arrays [a] and [b] is copied, so the list of buckets
   returned by [buckets a b] remains valid even if the arrays [a] and [b]
   are modified afterwards. *)

let buckets (a : suffix array) (b : bool array) : suffix array list =
  let n = length a in
  assert (n = length b);
  (* Prepare to emit a list of buckets. *)
  let buckets : suffix array list ref = ref [] in
  let emit bucket = buckets := bucket :: !buckets in
  let return () = List.rev !buckets in
  (* Emit a fictional singleton bucket whose element is the empty suffix. *)
  let bucket = make 1 n in
  emit bucket;
  (* The variable [l] contains the address of the beginning of the current
     bucket. The variable [i] is the index that we are currently scanning. *)
  let l = ref 0 in
  for i = 1 to n do
    if i = n || b.(i) then begin
      (* [i] has reached either the end of the array or the beginning of
         another bucket. In either case, one bucket has been identified:
         it extends from [!l] included to [i] excluded. Emit it. *)
      let bucket = Array.sub a !l (i - !l) in
      emit bucket;
      (* [i] is the beginning of the next bucket. *)
      l := i
    end
  done;
  return()

(* -------------------------------------------------------------------------- *)

(* This is an efficient implementation of sets of indices, where an index is
   an integer comprised in the semi-open interval [0, n). *)

(* It is used to implement a per-bucket allocator, which comes next. *)

module Set : sig

  (* An abstract type of sets of indices. *)
  type set

  (* [empty n] creates a new set that can store indices in the semi-open
     interval [0, n). This set is initially empty.
     The complexity of [empty n] is O(n). *)
  val empty: int -> set

  (* [mem x s] tests whether the index [x] is a member of the set [s].
     Its complexity is O(1). *)
  val mem: index -> set -> bool

  (* [add x s] inserts the index [x] into the set [s].
     Its complexity is O(1). *)
  val add: index -> set -> unit

  (* [clear s] empties the set [s].
     Its complexity is O(k), where [k] is the cardinality of [s].
     It is therefore faster than creating a new empty set. *)
  val clear: set -> unit

end = struct

  (* For efficiency, we use a redundant data representation. The array
     [present] allows testing in constant time whether an index is a member of
     the set. The list [members] allows iterating in linear time over the
     members of the set. *)

  type set = {
    present: bool array;
    mutable members: int list;
  }

  let empty n = {
    present = make n false;
    members = [];
  }

  let mem x s =
    assert (let n = length s.present in 0 <= x && x < n);
    s.present.(x)

  let add x s =
    assert (let n = length s.present in 0 <= x && x < n);
    if not s.present.(x) then begin
      s.present.(x) <- true;
      s.members <- x :: s.members
    end

  let clear s =
    s.members |> List.iter (fun x -> s.present.(x) <- false);
    s.members <- []

end (* Set *)

(* -------------------------------------------------------------------------- *)

(* This is an efficient per-bucket allocator. *)

(* We assume that the semi-open interval [0, n) has been divided into a number
   of buckets. Each bucket is identified by its start index [e]. *)

(* We view all of these buckets as initially empty, and we wish to fill every
   bucket with data, from left to right. To allow this, the allocator
   internally keeps track, for each bucket, of which slots have been filled
   already and which slots are still available. The function call
   [reserve_slot allocator e] returns the index of the first available slot in
   bucket [e] and marks this slot as unavailable. *)

(* Furthermore, we wish to perform this process in several rounds. Every time
   [end_round allocator] is called, the current round ends. The function call
   [reserve_slot allocator e] returns not only a slot index, as described
   above, but also a Boolean flag that tells whether this slot is the first
   slot that has been reserved in bucket [e] during the current round. *)

module Allocator : sig

  (* An abstract type of per-bucket allocators. *)
  type allocator

  (* [make n] creates a new per-bucket allocator for an array of size [n].
     Its complexity is O(n). *)
  val make: int -> allocator

  (* [reserve_slot allocator e] marks one more slot from the bucket [e] as
     unavailable. It returns the index of this slot and a flag that tells
     whether this is the first slot that was allocated from bucket [e] during
     this round. Its complexity is O(1). This call is permitted only if [e] is
     indeed the start index of a bucket and if there still exists a free slot
     in bucket [e]. *)
  val reserve_slot: allocator -> index -> index * bool

  (* [end_round allocator] declares the end of a round. Its complexity is
     O(k), where [k] is the number of slots that have been allocated during
     this round. Charging this cost to [reserve_slot] allows considering that
     [end_round] has amortized complexity O(1). *)
  val end_round: allocator -> unit

end = struct

  type allocator = {
    (* If [e] is the start index of a bucket, then [count.(e)] is the number
       of slots that have been allocated out of bucket [e] since this
       allocator was created. Otherwise, [count.(e)] is meaningless. *)
    count: index array;
    (* [touched] is a set of all buckets in which at least one allocation took
       place within this round. *)
    touched: Set.set;
  }

  let make n =
    let count = make n 0 in
    let touched = Set.empty n in
    { count; touched }

  let reserve_slot { count; touched } e =
    (* We do not check whether [e] is a valid bucket index and whether there
       still exists a free slot in this bucket. This information is not
       available to us. *)
    (* Allocate a new slot out of bucket [e]. *)
    let slot = e + count.(e) in
    count.(e) <- count.(e) + 1;
    (* Determine if this is the first time bucket [e] was touched during this
       round. If so, record the fact that bucket [e] was touched. *)
    let first = not (Set.mem e touched) in
    if first then Set.add e touched;
    (* Return. *)
    slot, first

  let end_round { touched; _ } =
    Set.clear touched

end

(* -------------------------------------------------------------------------- *)

(* We finally come to Question 8. (Sorry, the build-up was long.) *)

(* The runtime assertions below can be very useful while debugging (and you
   are welcome to add more assertions in your own code!), but some of these
   assertions are quite costly and destroy the algorithm's efficiency. Thus,
   we suggest setting [debug] to [true] until the algorithm appears to work,
   at which point [debug] can be set to [false]. *)

let debug =
(* BEGIN INCLUDE *)
  true
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  false
     END EXCLUDE *)

(* [stage s h a b] assumes that the array [a] is a suffix array at stage [h]
   with respect to the string [s], that is, the suffixes in the array [a] are
   sorted according to their [h]-prefixes. It further assumes that the array
   [b] contains beginning-of-bucket marks for these [h]-buckets. It sorts the
   array [a], in place, so as to make it a suffix array for the string [s]. *)

let rec stage (s : string) (h : int) (a : suffix array) (b : bool array) =
  let n = String.length s in
  if debug then begin
    assert (h > 0);
    assert (length a = n);
    assert (is_permutation a);
    let key i = prefix h (suffix s i) in
    assert (is_sorted (fun i j -> key i <= key j) a);
    assert (length b = n);
    assert (b = beginning_of_bucket key a)
  end;
(* BEGIN INCLUDE *)
  if raise TODO then (* TO DO: Find a suitable termination condition. *)
    a
  else begin
    (* For every suffix [d] in the range [0, n), [home d] must return (the
       index of the beginning of) the bucket where [d] lies. Its complexity
       must be O(1). It is permitted to perform O(n) preprocessing: that is,
       before defining [home], you can make auxiliary definitions whose cost
       is O(n). The function [home] must be defined in such a way that it is
       insensitive to modifications of the arrays [a] and [b]. *)
    let home (d : suffix) : index =
      (* TO DO: Define this local function. *)
      raise TODO
    in
    (* Prepare to write new elements into every bucket, and to divide every
       bucket into a number of sub-buckets. *)
    let allocator = Allocator.make n in
    (* Process the current buckets, one by one. The processing of each bucket
       represents a round. The buckets must be processed from left to right,
       and within each bucket, elements must be processed from left to right.
       As each suffix [d] is encountered, the suffix [d-h] (if it exists)
       must be written to an appropriate place by updating the arrays [a]
       and [b]. Thus, every suffix in the closed interval [0, n-h] is written
       once. The suffixes above [n-h] have already reached their final position
       and do not move. *)
    (* TO DO: Implement this loop. Once the loop ends, move on to the next
       stage. *)
    raise TODO
  end
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  (* First, test whether we are done. The condition [h >= n] obviously implies
     that [a] is already a suffix array. The condition [population b = n]
     indicates that we already have [n] distinct buckets; therefore, every
     bucket has exactly one element; therefore no elements can be moved any
     more, since an element can move only within its bucket. Thus, either of
     these conditions guarantees that we are done. In fact, [h >= n] implies
     [population b = n]: indeed, when [h >= n] holds, no two suffixes are
     [h]-equal, therefore no two suffixes can inhabit the same [h]-bucket.
     We could test just [population b = n], but the test [h >= n] is much
     faster, so we keep it as a fast path. We could also test just [h >= n],
     but that might cause us to perform unnecessary stages. *)
  if h >= n || population b = n then
    a
  else begin
    (* For every suffix [d] in the range [0, n), [home d] returns the
       index of (the beginning of) the bucket where [d] lies. Its complexity
       is O(1). Its definition relies on the arrays [rank] and [nsbl], whose
       construction costs O(n). The arrays [a] and [b] are consulted during
       the preprocessing phase only, so the function [home] is insensitive
       to modifications of these arrays. *)
    let rank, nsbl = invert a, nearest_set_bit_left b in
    let home (d : suffix) : index =
      nsbl.(rank.(d))
    in
    (* Prepare to write new elements into every bucket, and to divide every
       bucket into a number of sub-buckets. *)
    let allocator = Allocator.make n in
    (* Process the current buckets, one by one. The processing of each bucket
       represents a round. The buckets are processed from left to right,
       and within each bucket, elements are processed from left to right.
       As each suffix [d] is encountered, the suffix [d-h] (if it exists)
       is written to an appropriate place by updating the arrays [a]
       and [b]. Thus, every suffix in the closed interval [0, n-h] is written
       once. The suffixes above [n-h] have already reached their final position
       and do not move. *)
    buckets a b |> List.iter (fun bucket ->
      bucket |> Array.iter (fun d ->
        assert (0 <= d && d <= n);
        let d = d - h in
        if 0 <= d then begin
          assert (0 <= d && d <= n - h);
          (* Find out which [h]-bucket [e] the suffix [d] inhabits. *)
          let e = home d in
          (* Allocate a new slot from bucket [e] and write [d] into this slot
             by updating the array [a]. If this is the first allocation in
             bucket [e] during this round, then update the array [b] to as to
             indicate that this is the beginning of a new sub-bucket. *)
          let slot, first = Allocator.reserve_slot allocator e in
          a.(slot) <- d;
          if first then b.(slot) <- true
        end
      );
      Allocator.end_round allocator
    );
    (* Move on to the next stage. *)
    stage s (2*h) a b
  end
     END EXCLUDE *)

(* [suffix_sort s] returns a suffix array [a] for the string [s]. *)

let suffix_sort (s : string) : suffix array =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  (* Stage 0. Create an array [a] of all suffixes. Sort this array, based
     on the first character of each suffix. Create an array [b] indicating
     where each bucket begins. [m] is the size of the alphabet. *)
  let n = String.length s in
  let a : suffix array = init n (fun i -> i) in
  let key i = Char.code s.[i] in
  let m = 256 in
  pigeonhole_sort m key a;
  let b = beginning_of_bucket key a in
  (* Now move on to stages 1, 2, 4, 8, etc., as far as required. *)
  stage s 1 a b
     END EXCLUDE *)
