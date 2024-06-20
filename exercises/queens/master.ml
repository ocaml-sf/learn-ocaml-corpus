let rec select (xs : 'a list) : ('a * 'a list) m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  delay (fun () ->
    match xs with
    | [] ->
        fail
    | x :: xs ->
        choose
          (return (x, xs))
          (
            select xs >>= fun (winner, xs) ->
            return (winner, x :: xs)
          )
  )
     END EXCLUDE *)

let rec unfold : type a s . (s -> bool) -> (s -> (a * s) m) -> s -> a list m =
  fun final step s ->
(* BEGIN INCLUDE *)
    (* TO DO: Define this function. *)
    raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
    if final s then
      return []
    else
      step s >>= fun (x, s) ->
      unfold final step s >>= fun xs ->
      return (x :: xs)
     END EXCLUDE *)

(* BEGIN EXCLUDE
let is_empty xs =
  match xs with [] -> true | _ :: _ -> false

     END EXCLUDE *)
let permutations (xs : 'a list) : 'a list m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO (* one line *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  unfold is_empty select xs
     END EXCLUDE *)

(* BEGIN EXCLUDE
let rec nocdup1 x ys =
  match ys with
  | [] ->
      true
  | y :: ys ->
      x <> y && nocdup1 y ys

let nocdup xs =
  match xs with
  | [] ->
      true
  | x :: xs ->
      nocdup1 x xs

     END EXCLUDE *)
let nodup xs =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  nocdup (List.sort compare xs)
     END EXCLUDE *)

(* BEGIN EXCLUDE
let ups placement =
  List.mapi (+) placement

let downs placement =
  List.mapi (-) placement

     END EXCLUDE *)
let safe (placement : placement) : bool =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  nodup (ups placement) && nodup (downs placement)
     END EXCLUDE *)

(* BEGIN EXCLUDE
let rec up i j =
  if i < j then i :: up (i + 1) j else []

     END EXCLUDE *)
let queens (n : int) : placement m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  permutations (up 0 n) >>= fun placement ->
  if safe placement then return placement else fail
     END EXCLUDE *)

(* BEGIN EXCLUDE
module IntSet =
  Set.Make(struct
    type t = int
    let compare = compare
  end)

let occupy (x : int) (xs : IntSet.t) : IntSet.t m =
  if IntSet.mem x xs then
    fail
  else
    return (IntSet.add x xs)

type state =
  (IntSet.t * IntSet.t * int * int list)

let initial (n : int) : state =
  (IntSet.empty, IntSet.empty, 0, up 0 n)

let final (n : int) (_, _, i, _ : state) : bool =
  i = n

let step (uds, dds, i, js : state) : (int * state) m =
  select js >>= fun (j, js) ->
  let u, d = i + j, i - j in
  occupy u uds >>= fun uds ->
  occupy d dds >>= fun dds ->
  return (j, (uds, dds, i + 1, js))

     END EXCLUDE *)
let queenz (n : int) : placement m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  unfold (final n) step (initial n)
     END EXCLUDE *)
