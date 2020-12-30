let is_empty s =
  s.length = 0

let empty =
  let length = 0
  and get i = raise TODO
  and foreach k = raise TODO in
  { length; get; foreach }

let singleton x =
  let length = 1
  and get i = raise TODO
  and foreach k = raise TODO in
  { length; get; foreach }

let sum s1 s2 =
  let length =
    s1.length + s2.length
  and get i = raise TODO
  and foreach k = raise TODO
  in
  { length; get; foreach }

let product s1 s2 =
  let length =
    s1.length * s2.length
  and get i = raise TODO
  and foreach k = raise TODO
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
  and get i = raise TODO
  and foreach k = raise TODO in
  { length; get; foreach }

let length s =
  s.length

let get s i =
  s.get i

let foreach s k =
  s.foreach k
