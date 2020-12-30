open Printf
let iter = List.iter
let map = List.map
module T = Test_lib
module R = Report
type report = R.t
(* Determinism. *)
let () = Random.init 0

let grab ty name k =
  T.test_value (T.lookup_student ty name) k

exception Fail of report

(* This generic function takes as an argument the text of the message that
   will be displayed. A message is a list of inline things. *)

let fail (text : R.inline list) =
  let report = [R.Message (text, R.Failure)] in
  raise (Fail report)

(* [protect f] evaluates [f()], which either returns normally and produces a
   report, or raises [Fail] and produces a report. In either case, the report
   is returned. *)

(* If an unexpected exception is raised, in student code or in grading code,
   the exception is displayed as part of a failure report. (Ideally, grading
   code should never raise an exception!) It is debatable whether one should
   show just the name of the exception, or a full backtrace; I choose the
   latter, on the basis that more information is always preferable. *)

let protect f =
  try
    T.run_timeout f
  with
  | Fail report ->
      report
  | TODO ->
      let text = [
        R.Text "Not yet implemented."
      ] in
      let report = [R.Message (text, R.Failure)] in
      report
  | (e : exn) ->
      let text = [
        R.Text "The following exception is raised and never caught:";
        R.Break;
        R.Output (Printexc.to_string e);
        R.Output (Printexc.get_backtrace());
      ] in
      let report = [R.Message (text, R.Failure)] in
      report

(* -------------------------------------------------------------------------- *)

(* We want to allow the student to use random testing, so we cannot
   test the student's code (whose results won't be reproducible).
   Instead, we just ask the students for inputs which (reproducibly)
   cause incorrect behavior in our code. *)

(* For an added element of fun and challenge, we give a higher grade
   to smaller counter-examples. This introduces the need to perform
   iterative deepening (when doing exhaustive testing) or shrinking
   (when doing random testing). *)

(* The grading rules are:

   [Some xs] where [xs] is indeed a problematic input
     5 points if [xs] has minimal length
     2 points otherwise
   [None]
     0 points
   [Some xs] where [xs] is not a problematic input
     -5 points

 *)

let optimal = 5
let suboptimal = 2
let neutral = 0
let wrong = -5

let sorts : (int list -> int list) array =
  List.map (fun (_, _, sort) -> sort) candidates |> Array.of_list

let optimum : int array =
  List.map (fun (_, s, _) -> s) candidates |> Array.of_list

let test_input (i, score, messages) (input : int list option) =
  let points, message =
    match input with
    | None ->
        neutral,
        [ R.Text "You did not provide an input." ]
    | Some input ->
        let expected_output = List.sort compare input in
        let problematic : bool =
          match sorts.(i) input with
          | output ->
              output <> expected_output
          | exception _ ->
              true
        in
          if problematic then
            let achieved = List.length input
            and optimum = optimum.(i) in
            if achieved = optimum then
              (* problematic input of optimal size *)
              optimal,
              [ R.Text (sprintf "You provided a problematic input \
                                 of optimal size (%d)." optimum) ]
            else
              (* problematic input of suboptimal size *)
              suboptimal,
              [ R.Text (sprintf "You provided a problematic input \
                                 of size %d, which is good, \
                                 but there exist shorter ones." achieved) ]
          else
            (* non-problematic input *)
            wrong,
            [ R.Text ("The input that you provided \
                       does not reveal a problem! This is bad.") ]
  in
  let message =
    R.Break ::
    R.Text (sprintf "Candidate %d:" i) ::
    R.Break ::
    message
  in
  (i + 1, score + points, message :: messages)

let test_inputs inputs =
  let m = List.length inputs
  and n = Array.length sorts in
  if m <> n then
    fail [
      R.Text "The length of the list"; R.Code "inputs";
      R.Text (sprintf "is %d, whereas it should be %d." m n);
    ];
  let _, score, messages =
    List.fold_left test_input (0, 0, []) inputs
  in
  let messages =
    List.flatten (List.rev messages)
  in
  let messages =
    R.Text (sprintf "Your score is %d." score) ::
    R.Text (sprintf "The maximum score is %d." (n * optimal)) ::
    messages
  in
  [ R.Message (messages, R.Success score) ]

let test_inputs () =
  grab [%ty: int list option list] "inputs" (fun inputs ->
    protect (fun () ->
      test_inputs inputs
    )
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_inputs() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
