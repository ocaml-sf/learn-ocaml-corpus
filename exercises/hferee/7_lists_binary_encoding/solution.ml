let rec is_binary = function
  (* non vide *)
  | Nothing -> false
    (* zéro et un marchent *)
  | OneMore(_, Nothing) -> true
    (* seul zéro termine par zero *)
  | OneMore(_, OneMore(false, Nothing)) -> false
  | OneMore(h, t) -> is_binary t

let rec encode = function
  | n when n < 0 -> failwith "négatif"
  | 0 -> OneMore(false, Nothing)
  | 1 -> OneMore(true, Nothing)
  | n -> OneMore(n mod 2 = 1 , encode (n / 2))

let rec decode = function
  | Nothing -> 0
  | OneMore(h, t) -> (if h then 1 else 0) + 2 * decode t

let half_adder x y =
  let xandy = x && y in
  (x || y) && (not xandy), xandy

  let plus_bin =
  let rec plus_carry c l1 l2 =
    match c, l1,  l2 with
    | false, l, Nothing | false, Nothing, l -> l
    | true, l, Nothing | true, Nothing, l -> plus_carry false (OneMore(true, Nothing)) l
    | c, OneMore(x, l1), OneMore(y, l2) ->
      let b1, c1 = half_adder c x in
      let b, c2 = half_adder b1 y in
      OneMore(b, plus_carry (c1 || c2) l1 l2)
  in plus_carry false
