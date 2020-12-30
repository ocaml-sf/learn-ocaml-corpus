let return (x : 'a) : 'a m =
  let compute s f = s x f in
  { compute }
  (* We are able to produce the result [x]. To do so, we invoke the
     success continuation [s] with [x]. Because we are unable to produce
     more results, if the outside world requests more results from us,
     we must fail; to indicate this, we directly pass our own failure
     continuation [f] to [s]. This is a tail call in CPS style. *)

let (>>=) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
  let compute s f = m1.compute (fun x f' -> (m2 x).compute s f') f in
  { compute }
  (* In order to produce anything, we must first execute [m1]. If it
     fails, we fail as well; we indicate this by passing [f] as the
     failure continuation. Every time [m1] produces an element [x],
     we execute the computation [m2 x]. Every result produced by it
     is passed directly to our success continuation [s]. If and when
     [m2 x] fails, we must request the next [x], which is done by
     calling [f'], the failure continuation that came with [x]. *)

let fail : 'a m =
  let compute s f = f() in
  { compute }
  (* We are unable to produce any result, so we immediately invoke
     the failure continuation [f]. *)

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
  let compute s f = m1.compute s (fun () -> m2.compute s f) in
  { compute }
  (* We first execute [m1] and let it pass each of its results directly
     to [s]. Once it is done, it fails and calls the failure continuation
     that we have built. There, we execute [m2] and let it pass each of
     its results directly to [s]. Once it is done, it fails; we provide
     our own failure continuation [f] to it, so that this failure propagates
     upwards without passing through us. *)

let sols (m : 'a m) : 'a Seq.t =
  fun () ->
    m.compute
      (fun x f -> Seq.Cons (x, f))
      (fun () -> Seq.Nil)

let reflect (o : unit -> ('a * 'a m) option) : 'a m =
  delay (fun () ->
    match o() with
    | None ->
        fail
    | Some (x, m) ->
        choose (return x) m
  )

let msplit (m : 'a m) : unit -> ('a * 'a m) option =
  fun () ->
    m.compute
      (fun x f -> Some (x, reflect f))
      (fun  () -> None)

let at_most_once (m : 'a m) : 'a m =
  delay (fun () ->
    match msplit m () with
    | None ->
        fail
    | Some (x, _) ->
        return x
  )

let rec interleave (m1 : 'a m) (m2 : 'a m) : 'a m =
  delay (fun () ->
    match msplit m1 () with
    | None ->
        m2
    | Some (x1, m1) ->
        choose (return x1) (interleave m2 m1)
  )

let rec (>>-) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
  delay (fun () ->
    match msplit m1 () with
    | None ->
        fail
    | Some (x1, m1) ->
        interleave (m2 x1) (m1 >>- m2)
  )
