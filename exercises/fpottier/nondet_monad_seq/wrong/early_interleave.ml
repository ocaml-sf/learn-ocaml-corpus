let return (x : 'a) : 'a m =
  Seq.singleton x

let (>>=) (m1 : 'a m) (f2 : 'a -> 'b m) : 'b m =
  m1 |> Seq.map f2 |> Seq.flatten

let fail : 'a m =
  Seq.nil

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
  Seq.concat m1 m2

let sols (m : 'a m) : 'a Seq.t =
  m

let at_most_once (m : 'a m) : 'a m =
  Seq.take 1 m

let rec interleave (m1 : 'a m) (m2 : 'a m) : 'a m =
  match m1() with
  | Seq.Nil ->
      m2
  | Seq.Cons (x1, m1) ->
      Seq.cons x1 (interleave m2 m1)
