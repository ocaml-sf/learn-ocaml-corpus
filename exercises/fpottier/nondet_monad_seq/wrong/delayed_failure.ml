exception Gotcha

let gotcha : 'a m =
  delay (fun () -> raise Gotcha)
  (* wrong: this fails when the sequence is forced --
     even though it appears to succeed at construction time. *)

let return (x : 'a) : 'a m =
  gotcha

let (>>=) (m1 : 'a m) (f2 : 'a -> 'b m) : 'b m =
  gotcha

let fail : 'a m =
  gotcha

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
  gotcha

let sols (m : 'a m) : 'a Seq.t =
  m
