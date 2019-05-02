let return (x : 'a) : 'a m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  Seq.singleton x
     END EXCLUDE *)

let (>>=) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  m1 |> Seq.map m2 |> Seq.flatten
     END EXCLUDE *)

let fail : 'a m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this constant. (Delete the whole line.) *)
  delay (fun () -> raise TODO)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  Seq.nil
     END EXCLUDE *)

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  Seq.concat m1 m2
     END EXCLUDE *)

let sols (m : 'a m) : 'a Seq.t =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  m
     END EXCLUDE *)

let at_most_once (m : 'a m) : 'a m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  Seq.take 1 m
     END EXCLUDE *)

let rec interleave (m1 : 'a m) (m2 : 'a m) : 'a m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  delay (fun () ->
    match m1() with
    | Seq.Nil ->
        m2
    | Seq.Cons (x1, m1) ->
        choose (return x1) (interleave m2 m1)
  )
     END EXCLUDE *)

let rec (>>-) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  delay (fun () ->
    match m1() with
    | Seq.Nil ->
        fail
    | Seq.Cons (x1, m1) ->
        interleave (m2 x1) (m1 >>- m2)
  )
     END EXCLUDE *)
