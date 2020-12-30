let return (x : 'a) : 'a m =
  Seq.singleton x

let (>>=) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
  m1 |> Seq.map m2 |> Seq.flatten

let fail : 'a m =
  Seq.nil

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
  Seq.concat m1 m2

let sols (m : 'a m) : 'a Seq.t =
  m

let at_most_once (m : 'a m) : 'a m =
  Seq.take 1 m

let rec interleave (m1 : 'a m) (m2 : 'a m) : 'a m =
  delay (fun () ->
    match m1() with
    | Seq.Nil ->
        m2
    | Seq.Cons (x1, m1) ->
        choose (return x1) (interleave m2 m1)
  )

let rec (>>-) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
  delay (fun () ->
    match m1() with
    | Seq.Nil ->
        fail
    | Seq.Cons (x1, m1) ->
        interleave (m2 x1) (m1 >>- m2)
  )
