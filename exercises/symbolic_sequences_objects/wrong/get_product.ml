let is_empty s =
  s.length = 0

let empty =
  let length = 0
  and get _ = raise OutOfBounds
  and foreach _k = () in
  { length; get; foreach }

let singleton x =
  let length = 1
  and get i = if i = 0 then x else raise OutOfBounds
  and foreach k = k x in
  { length; get; foreach }

let sum s1 s2 =
  let length =
    s1.length + s2.length
  and get i =
    if i < s1.length then s1.get i
    else s2.get (i - s1.length)
  and foreach k =
    s1.foreach k;
    s2.foreach k
  in
  { length; get; foreach }

let product s1 s2 =
  let length =
    s1.length * s2.length
  and get i =
    let q, r = i / s2.length, i mod s2.length in
    s1.get r, s2.get q (* wrong *)
  and foreach k =
    s1.foreach (fun x1 ->
      s2.foreach (fun x2 ->
        k (x1, x2)
      )
    )
  in
  { length; get; foreach }

let product s1 s2 =
  if is_empty s2 then
    (* Guard against division by zero in the above code. *)
    (* Such a division could occur on an out-of-bounds access. *)
    empty
  else
    product s1 s2

let map phi s =
  let length = s.length
  and get i = phi (s.get i)
  and foreach k = s.foreach (fun x -> k (phi x)) in
  { length; get; foreach }

let length s =
  s.length

let get s i =
  s.get i

let foreach s k =
  s.foreach k
