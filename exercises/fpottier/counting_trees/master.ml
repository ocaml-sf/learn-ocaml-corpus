(* BEGIN INCLUDE
(* TO DO: Define [weight] and [height]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
let rec weight (t : tree) =
  match t with
  | Leaf ->
      0
  | Node (t1, t2) ->
      weight t1 + weight t2 + 1

let rec height (t : tree) =
  match t with
  | Leaf ->
      0
  | Node (t1, t2) ->
      max (height t1) (height t2) + 1
(* END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: Define [naive_trees_of_weight]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
(* Counting the binary trees of weight [w], in a naive way.
   Not efficient (no memoization) and not elegant (explicit
   mutable partial sum). *)

let rec naive_trees_of_weight w =
  if w = 0 then
    1
  else
    let w = w - 1 in
    let sum = ref 0 in
    for w1 = 0 to w do
      let w2 = w - w1 in
      sum := !sum + naive_trees_of_weight w1 * naive_trees_of_weight w2
    done;
    !sum
(* END EXCLUDE *)

(* BEGIN INCLUDE *)
(* TO DO: Define [trees_of_weights]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
(* Counting the binary trees of weight [i], for every [i] ranging
   from [0] to [w], using dynamic programming. An array is used to
   store the results. *)

let trees_of_weights w =
  let trees_of_weight = Array.make (w+1) 0 in
  (* Base case. *)
  trees_of_weight.(0) <- 1;
  (* Step. *)
  for i = 1 to w do
    for i1 = 0 to i - 1 do
      let i2 = i - 1 - i1 in
      trees_of_weight.(i) <- trees_of_weight.(i) +
        trees_of_weight.(i1) * trees_of_weight.(i2)
    done
  done;
  (* Done. *)
  trees_of_weight
(* END EXCLUDE *)

(* The function call [fix ff] creates and returns a new memoizing function [f]
   of type [a -> b]. Its argument [ff] is itself a function, whose type is
   [(a -> b) -> (a -> b)], and which expects to receive [f] as an argument. *)

(* For example, a memoizing factorial can be defined using [fix] as
   follows:

    let fact =
      fix (fun self ->
        fun n ->
          if n = 0 then 1 else n * self(n-1)
      )

   Note that the definition of [fact] does not involve [let rec]. Instead,
   when we wish to perform a recursive call, we invoke [self], the memoizing
   function which [fix] has constructed for us. *)

(* We assume that it is permitted to use values of type [a] as keys in a hash
   table. That is, we assume that OCaml's generic equality and hash functions
   behave in a sensible way when applied to values of type [a]. Thus, we can
   use the functions [create], [add], [find] provided by the module [Hashtbl]
   in OCaml's standard library. *)
(* BEGIN INCLUDE *)

(* It is now up to you to implement [fix], based on the following skeleton. *)
(*   END INCLUDE *)

let fix : type a b . ((a -> b) -> (a -> b)) -> (a -> b) =
  fun ff ->
    let table = Hashtbl.create 128 in
    let rec f (x : a) : b =
(* BEGIN INCLUDE *)
      (* TO DO: complete this code. *)
      raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
      try
        Hashtbl.find table x
      with Not_found ->
        let y = ff f x in
        Hashtbl.add table x y;
        y
    END EXCLUDE *)
    in
    f

(* BEGIN INCLUDE
(* TO DO: Define [sigma]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
(* Summing a function [f] over a closed integer interval [i..j]. *)

let sigma i j f =
  let sum = ref 0 in
  for x = i to j do
    sum := !sum + f x
  done;
  !sum
     END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: Define [split_weight]. *)

(* TO DO: Define [trees_of_weight]. *)

(* TO DO: Define [trees_of_weight_0_19]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
(* Summing over all ways of splitting a weight [w] into [w1+w2]. *)

(* We do not assume that [f] is symmetric. If we did, then we could
   cut in half the number of calls to [f]. *)

let split_weight w f =
  sigma 0 w (fun w1 ->
    let w2 = w - w1 in
    f w1 w2
  )

(* Counting the binary trees of weight [w]. *)

let trees_of_weight =
  fix (fun trees_of_weight w ->
    if w = 0 then
      1
    else
      split_weight (w - 1) (fun w1 w2 ->
        trees_of_weight w1 * trees_of_weight w2
      )
  )

(* Tabulating a function on the closed interval [i..j]. *)

let rec tabulate (f : int -> 'a) i j : 'a list =
  if i <= j then
    f i :: tabulate f (i+1) j
  else
    []

let trees_of_weight_0_19 =
  tabulate trees_of_weight 0 19
  (* [1; 1; 2; 5; 14; 42; 132; 429; 1430; 4862; 16796; 58786; 208012; 742900;
      2674440; 9694845; 35357670; 129644790; 477638700; 1767263190] *)
  (* The [Catalan numbers](https://oeis.org/A000108) *)
     END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: Define [split_wb_weight]. *)

(* TO DO: Define [wb_trees_of_weight]. *)

(* TO DO: Define [wb_trees_of_weight_0_19]. *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
(* Summing over all ways of splitting a weight [w] into [w1+w2],
   while ensuring that [w1] and [w2] differ by at most one. *)

let split_wb_weight w f =
  if w mod 2 = 0 then
    (* If [w] is even, there is only possibility: the weight must
       be evenly divided. *)
    let w = w/2 in
    f w w
  else
    (* Otherwise, there are two possibilities: the excess weight
       is either on the left-hand side or on the right-hand side. *)
    let w = w/2 in
    f (w+1) w + f w (w+1)

(* Counting the weight-balanced binary trees of weight [w]. *)

let wb_trees_of_weight =
  fix (fun wb_trees_of_weight w ->
    if w = 0 then
      1
    else
      split_wb_weight (w - 1) (fun w1 w2 ->
        wb_trees_of_weight w1 * wb_trees_of_weight w2
      )
  )

let wb_trees_of_weight_0_19 =
  tabulate wb_trees_of_weight 0 19
  (* [1; 1; 2; 1; 4; 4; 4; 1; 8; 16; 32; 16; 32; 16; 8; 1; 16; 64; 256; 256] *)
  (* https://oeis.org/A110316 *)
  (* Quite an amazing sequence! *)
  (* [wb_trees_of_weight s] is always a power of two. *)
  (* If [s] is a power of two minus 1, then [wb_trees_of_weight s] is 1.
     There is only one balanced tree in that case, and it is perfectly balanced. *)
  END EXCLUDE *)
