let force (m : 'a m) : 'a m =
  Seq.of_list (Seq.to_list m)

let return (x : 'a) : 'a m =
  Seq.singleton x

let (>>=) (m1 : 'a m) (f2 : 'a -> 'b m) : 'b m =
  force (m1 |> Seq.map f2 |> Seq.flatten)

let fail : 'a m =
  Seq.nil

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
  Seq.concat m1 m2

let sols (m : 'a m) : 'a Seq.t =
  m
