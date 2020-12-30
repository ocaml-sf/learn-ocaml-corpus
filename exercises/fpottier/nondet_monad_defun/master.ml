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
        (* BEGIN INCLUDE *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        compute m2 s f
             END EXCLUDE *)
    | FSols ->
        (* Used at the top level of [sols].
           Invoked when there is no result. *)
        (* BEGIN INCLUDE *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        Seq.Nil
             END EXCLUDE *)
    | FSplit ->
        (* Used at the top level of [msplit].
           Invoked when there is no result. *)
        (* BEGIN INCLUDE *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        None
             END EXCLUDE *)

and apply_success : type a answer .
  (a, answer) success -> a -> answer failure -> answer
=
  fun s x f ->
    match s with
    | SBind (m2, s) ->
        (* Invoked when the left branch of a sequence produces a result [x].
           [m2] is the right-hand side of the sequence.
           [s] is the success continuation of the sequence. *)
        (* BEGIN INCLUDE *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        compute (m2 x) s f
             END EXCLUDE *)
    | SSols ->
        (* Used at the top level of [sols].
           Invoked when the computation produces a result [x]. *)
        (* Hint: construct a sequence whose head is [x] and whose
                 tail is [f], converted to the type of a sequence.
                 There is an easy way of performing this conversion. *)
        (* BEGIN INCLUDE *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        Seq.Cons (x, apply_failure f)
             END EXCLUDE *)
    | SSplit ->
        (* Used at the top level of [msplit].
           Invoked when the computation produces a result [x]. *)
        (* Hint: construct an optional-pair whose first component is [x]
                 and whose second component is [f], converted to the type
                 of a computation. There is an easy way of performing this
                 conversion.  *)
        (* BEGIN INCLUDE *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        Some (x, MReflect f)
             END EXCLUDE *)

and compute : type a answer .
  a m -> (a, answer) success -> answer failure -> answer
=
  fun m s f ->
    match m with
    | MDelay m ->
        (* BEGIN INCLUDE *)
        (* Hint: evaluate [m()] and continue. *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        compute (m()) s f
             END EXCLUDE *)
    | MReturn x ->
        (* BEGIN INCLUDE *)
        (* Hint: successfully produce the result [x]. *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        apply_success s x f
             END EXCLUDE *)
    | MBind (m1, m2) ->
        (* BEGIN INCLUDE *)
        (* Hint: enter the left-hand side [m1] with an
                 appropriate success continuation. *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        compute m1 (SBind (m2, s)) f
             END EXCLUDE *)
    | MFail ->
        (* BEGIN INCLUDE *)
        (* Hint: fail. *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        apply_failure f ()
             END EXCLUDE *)
    | MChoose (m1, m2) ->
        (* BEGIN INCLUDE *)
        (* Hint: enter the left-hand side [m1] with an
                 appropriate failure continuation. *)
        raise TODO (* one line *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        compute m1 s (FChoose (m2, s, f))
             END EXCLUDE *)
    | MReflect k ->
        (* BEGIN INCLUDE *)
        (* We have a failure continuation [k], which was captured earlier and
           wrapped with [MReflect] so as to pretend it is a computation. We are
           now asked to execute this computation. Thus, we must call this
           failure continuation, whose answer type is [('a * 'a m) option].
           Depending on whether this yields [None] or [Some (x, m)], we must
           behave either like [fail] or like [choose (return x) m]. *)
        raise TODO (* five lines *)
        (*   END INCLUDE *)
        (* BEGIN EXCLUDE
        match apply_failure k () with
        | None ->
            apply_failure f ()
            (* inlined version of [compute fail s f] *)
        | Some (x, m) ->
            apply_success s x (FChoose (m, s, f))
            (* inlined version of [compute (choose (return x) m) s f] *)
             END EXCLUDE *)

let sols (m : 'a m) : 'a Seq.t =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO (* one line *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  fun () ->
    compute m SSols FSols
     END EXCLUDE *)

let msplit (m : 'a m) : unit -> ('a * 'a m) option =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO (* one line *)
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  fun () ->
    compute m SSplit FSplit
     END EXCLUDE *)

let at_most_once (m : 'a m) : 'a m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  delay (fun () ->
    match msplit m () with
    | None ->
        fail
    | Some (x, _) ->
        return x
  )
     END EXCLUDE *)

let rec interleave (m1 : 'a m) (m2 : 'a m) : 'a m =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  delay (fun () ->
    match msplit m1 () with
    | None ->
        m2
    | Some (x1, m1) ->
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
    match msplit m1 () with
    | None ->
        fail
    | Some (x1, m1) ->
        interleave (m2 x1) (m1 >>- m2)
  )
     END EXCLUDE *)
