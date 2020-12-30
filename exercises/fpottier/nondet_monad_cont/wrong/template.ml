let return (x : 'a) : 'a m =
  (* TO DO: Define this function. *)
  raise TODO

let (>>=) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
  (* TO DO: Define this function. *)
  raise TODO

let fail : 'a m =
  (* TO DO: Define this constant. (Delete the whole line.) *)
  delay (fun () -> raise TODO)

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
  (* TO DO: Define this function. *)
  raise TODO

let sols (m : 'a m) : 'a Seq.t =
  (* TO DO: Define this function. *)
  raise TODO

let reflect (o : unit -> ('a * 'a m) option) : 'a m =
  delay (fun () ->
    match o() with
    | None ->
        (* TO DO: Complete this definition. *)
        raise TODO
    | Some (x, m) ->
        (* TO DO: Complete this definition. *)
        raise TODO
  )

let msplit (m : 'a m) : unit -> ('a * 'a m) option =
  fun () ->
    m.compute
      (* TO DO: Complete this definition. *)
      (fun x f -> raise TODO)
      (fun  () -> raise TODO)

let at_most_once (m : 'a m) : 'a m =
  (* TO DO: Define this function. *)
  raise TODO

let rec interleave (m1 : 'a m) (m2 : 'a m) : 'a m =
  (* TO DO: Define this function. *)
  raise TODO

let rec (>>-) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
  (* TO DO: Define this function. *)
  raise TODO
