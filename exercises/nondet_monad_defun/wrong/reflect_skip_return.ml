(* The following four constructor functions are implemented already. *)

let return (x : 'a) : 'a m =
  MReturn x

let (>>=) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
  MBind (m1, m2)

let fail : 'a m =
  MFail

let choose (m1 : 'a m) (m2 : 'a m) : 'a m =
  MChoose (m1, m2)

(* The bulk of the work is to implement an interpreter for each of the
   data types [_ failure], [(_, _) success], and [_ m]. *)

let rec apply_failure : type answer . answer failure -> unit -> answer =
  fun f () ->
    match f with
    | FChoose (m2, s, f) ->
        (* Invoked when the left branch of a choice has failed.
           [m2] is the right branch of the choice.
           [s] and [f] are the continuations of the choice. *)
        compute m2 s f
    | FSols ->
        (* Used at the top level of [sols].
           Invoked when there is no result. *)
        Seq.Nil
    | FSplit ->
        (* Used at the top level of [msplit].
           Invoked when there is no result. *)
        None

and apply_success : type a answer .
  (a, answer) success -> a -> answer failure -> answer
=
  fun s x f ->
    match s with
    | SBind (m2, s) ->
        (* Invoked when the left branch of a sequence produces a result [x].
           [m2] is the right-hand side of the sequence.
           [s] is the success continuation of the sequence. *)
        compute (m2 x) s f
    | SSols ->
        (* Used at the top level of [sols].
           Invoked when the computation produces a result [x]. *)
        (* Hint: construct a sequence whose head is [x] and whose
                 tail is [f], converted to the type of a sequence.
                 There is an easy way of performing this conversion. *)
        Seq.Cons (x, apply_failure f)
    | SSplit ->
        (* Used at the top level of [msplit].
           Invoked when the computation produces a result [x]. *)
        (* Hint: construct an optional-pair whose first component is [x]
                 and whose second component is [f], converted to the type
                 of a computation. There is an easy way of performing this
                 conversion.  *)
        Some (x, MReflect f)

and compute : type a answer .
  a m -> (a, answer) success -> answer failure -> answer
=
  fun m s f ->
    match m with
    | MDelay m ->
        compute (m()) s f
    | MReturn x ->
        apply_success s x f
    | MBind (m1, m2) ->
        compute m1 (SBind (m2, s)) f
    | MFail ->
        apply_failure f ()
    | MChoose (m1, m2) ->
        compute m1 s (FChoose (m2, s, f))
    | MReflect k ->
        match apply_failure k () with
        | None ->
            apply_failure f ()
            (* inlined version of [compute fail s f] *)
        | Some (x, m) ->
            compute m s f (* wrong: skip returning [x] *)

let sols (m : 'a m) : 'a Seq.t =
  fun () ->
    compute m SSols FSols

let msplit (m : 'a m) : unit -> ('a * 'a m) option =
  fun () ->
    compute m SSplit FSplit

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
