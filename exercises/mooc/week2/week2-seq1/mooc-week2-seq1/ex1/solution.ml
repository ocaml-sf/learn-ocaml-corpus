let pairwise_distinct (lup, rup, llp, rlp) =
  lup <> rup && lup <> llp && lup <> rlp
  && rup <> llp && rup <> rlp
  && llp <> rlp
let wellformed ((lup, rup, llp, rlp) as tetragon) =
  let property_a =
    fst lup < fst rup
    && fst lup < fst rlp
    && fst llp < fst rup
    && fst llp < fst rlp
  and property_b =
    snd lup > snd llp
  and property_c =
    snd rup > snd rlp
  in
  pairwise_distinct tetragon
  && property_a && property_b && property_c
let rotate_point (x, y) = (y, -x)
let reorder (p1, p2, p3, p4) =
  (* Determine the leftmost points (and therefore the rightmost points) *)
  let rec leftmosts (((x1, y1) as p1), ((x2, y2) as p2), ((x3, y3) as p3), ((x4, y4) as p4)) =
    if x1 <= x2 && x1 <= x3 && x1 <= x4 then (* x1 is one of the leftmost. *)
      if x2 < x3 && x2 < x4 then (* x2 is the other one. *)
	(p1, p2, p3, p4)
      else if x3 < x4 && x3 < x2 then (* x3 is the other one. *)
	(p1, p3, p2, p4)
      else
	(p1, p4, p2, p3)
    else
      leftmosts (p2, p3, p4, p1)
  in
  let leftmost1, leftmost2, rightmost1, rightmost2 =
    leftmosts (p1, p2, p3, p4)
  in
  let llp, lup =
    if snd leftmost1 < snd leftmost2 then
      (leftmost1, leftmost2)
    else
      (leftmost2, leftmost1)
  and rlp, rup =
    if snd rightmost1 < snd rightmost2 then
      (rightmost1, rightmost2)
    else
      (rightmost2, rightmost1)
  in
  let tetragon = (lup, rup, llp, rlp) in
  assert (wellformed tetragon);
  tetragon

let rotate_tetragon ((lup, rup, llp, rlp) as tetragon) =
  assert (wellformed tetragon);
  let lup' = rotate_point lup and rup' = rotate_point rup
  and llp' = rotate_point llp and rlp' = rotate_point rlp in
  let tetragon' = reorder (lup', llp', rup', rlp') in
  assert (wellformed tetragon');
  tetragon'
