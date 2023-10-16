let rec select (xs : 'a list) : ('a * 'a list) m =
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

let rec unfold : type a s . (s -> bool) -> (s -> (a * s) m) -> s -> a list m =
  fun final step s ->
    if final s then
      return []
    else
      step s >>= fun (x, s) ->
      unfold final step s >>= fun xs ->
      return (x :: xs)

let is_empty xs =
  match xs with [] -> true | _ :: _ -> false

let permutations (xs : 'a list) : 'a list m =
  unfold is_empty select xs

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

let nodup xs =
  nocdup (List.sort compare xs)

let ups placement =
  List.mapi (+) placement

let downs placement =
  List.mapi (-) placement

let safe (placement : placement) : bool =
  nodup (ups placement) && nodup (downs placement)

let rec up i j =
  if i < j then i :: up (i + 1) j else []

let queens (n : int) : placement m =
  permutations (up 0 n) >>= fun placement ->
  if safe placement then return placement else fail

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

let queenz (n : int) : placement m =
  unfold (final n) step (initial n)
