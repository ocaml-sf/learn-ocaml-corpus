let return (x : 'a) : 'a m =
  Seq.singleton x

let (>>=) (m1 : 'a m) (f2 : 'a -> 'b m) : 'b m =
  (* Not sure how to make a genuine mistake here,
     but here is an attempt: *)
  fun () ->
    match m1() with
    | Seq.Nil ->
        Seq.Nil
    | Seq.Cons (x1, _) -> (* wrong *)
        f2 x1 ()

let fail : 'a m =
  Seq.nil

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
  Seq.concat m1 m2

let sols (m : 'a m) : 'a Seq.t =
  m
