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
        raise TODO (* one line *)
    | FSols ->
        (* Used at the top level of [sols].
           Invoked when there is no result. *)
        raise TODO (* one line *)
    | FSplit ->
        (* Used at the top level of [msplit].
           Invoked when there is no result. *)
        raise TODO (* one line *)

and apply_success : type a answer .
  (a, answer) success -> a -> answer failure -> answer
=
  fun s x f ->
    match s with
    | SBind (m2, s) ->
        (* Invoked when the left branch of a sequence produces a result [x].
           [m2] is the right-hand side of the sequence.
           [s] is the success continuation of the sequence. *)
        raise TODO (* one line *)
    | SSols ->
        (* Used at the top level of [sols].
           Invoked when the computation produces a result [x]. *)
        (* Hint: construct a sequence whose head is [x] and whose
                 tail is [f], converted to the type of a sequence.
                 There is an easy way of performing this conversion. *)
        raise TODO (* one line *)
    | SSplit ->
        (* Used at the top level of [msplit].
           Invoked when the computation produces a result [x]. *)
        (* Hint: construct an optional-pair whose first component is [x]
                 and whose second component is [f], converted to the type
                 of a computation. There is an easy way of performing this
                 conversion.  *)
        raise TODO (* one line *)

and compute : type a answer .
  a m -> (a, answer) success -> answer failure -> answer
=
  fun m s f ->
    match m with
    | MDelay m ->
        (* Hint: evaluate [m()] and continue. *)
        raise TODO (* one line *)
    | MReturn x ->
        (* Hint: successfully produce the result [x]. *)
        raise TODO (* one line *)
    | MBind (m1, m2) ->
        (* Hint: enter the left-hand side [m1] with an
                 appropriate success continuation. *)
        raise TODO (* one line *)
    | MFail ->
        (* Hint: fail. *)
        raise TODO (* one line *)
    | MChoose (m1, m2) ->
        (* Hint: enter the left-hand side [m1] with an
                 appropriate failure continuation. *)
        raise TODO (* one line *)
    | MReflect k ->
        (* We have a failure continuation [k], which was captured earlier and
           wrapped with [MReflect] so as to disguise it as a computation. We are
           now asked to execute this computation. Thus, we must call this
           failure continuation, whose answer type is [('a * 'a m) option].
           Depending on whether this yields [None] or [Some (x, m)], we must
           behave either like [fail] or like [choose (return x) m]. *)
        raise TODO (* five lines *)

let sols (m : 'a m) : 'a Seq.t =
  (* TO DO: Define this function. *)
  raise TODO (* one line *)

let msplit (m : 'a m) : unit -> ('a * 'a m) option =
  (* TO DO: Define this function. *)
  raise TODO (* one line *)

let at_most_once (m : 'a m) : 'a m =
  (* TO DO: Define this function. *)
  raise TODO

let rec interleave (m1 : 'a m) (m2 : 'a m) : 'a m =
  (* TO DO: Define this function. *)
  raise TODO

let rec (>>-) (m1 : 'a m) (m2 : 'a -> 'b m) : 'b m =
  (* TO DO: Define this function. *)
  raise TODO
