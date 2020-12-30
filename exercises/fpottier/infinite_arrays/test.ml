open Printf
let iter = List.iter
let map = List.map
module T = Test_lib
module R = Report
type report = R.t
(* Determinism. *)
let () = Random.init 0

(* The auto-grader. *)

(* -------------------------------------------------------------------------- *)

(* Some of the code below should move to separate library files. *)

(* -------------------------------------------------------------------------- *)

(* List utilities. *)

let rec take n xs =
  match n, xs with
  | 0, _
  | _, [] ->
      []
  | _, x :: xs ->
      x :: take (n-1) xs

(* Careful with [shrink1]: its complexity is quadratic. *)

exception CannotShrink

let rec shrink1 (xs : 'a list) : 'a list list =
  match xs with
  | [] ->
      (* The empty list cannot be shrunk. *)
      raise CannotShrink
  | [x] ->
      [[]]
  | x :: xs ->
      (* To shrink the list [x :: xs], one must either remove [x] or keep [x]
         and shrink [xs]. *)
      xs :: map (fun xs -> x :: xs) (shrink1 xs)

(* -------------------------------------------------------------------------- *)

(* Generic testing utilities. *)

(* When we fail, the exception carries a learn-ocaml report. *)

exception Fail of report

(* [section title report] encloses the report [report] within a section
   entitled [title], producing a larger report. *)

let section title report : report =
  [R.Section ([R.Text title], report)]

(* This generic function takes as an argument the text of the message that
   will be displayed. A message is a list of inline things. *)

let fail (text : R.inline list) =
  let report = [R.Message (text, R.Failure)] in
  raise (Fail report)

(* This is a special case where the message is a singleton list containing
   a single string. The string can be formatted using a printf format. *)

let fail_text format =
  Printf.ksprintf (fun s -> fail [R.Text s]) format

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

(* Generic test functions. *)

let grab ty name k =
  T.test_value (T.lookup_student ty name) k

let text (s : string) =
  [ R.Text s ]

let success elements =
  [ R.Message (elements, R.Success 1) ]

let show_actual_behavior show_value behavior =
  match behavior with
  | Ok v ->
      R.Text "produces the following result:" ::
      R.Output (show_value v) ::
      []
  | Error e ->
      R.Text "raises the following exception:" ::
      R.Output (Printexc.to_string e) ::
      []

let something_is_wrong =
  R.Text "Something is wrong." ::
  []

let show_unit () =
  "()"

(* -------------------------------------------------------------------------- *)

(* Testing the student's code. *)

(* Instead of providing the type definition and testing the functions one by
   one, as we usually do, we ask the student to provide the type ['a t] and
   the three functions [make], [get], and [set], and grab them all at once. *)

module type INFINITE_ARRAYS = sig
  type 'a t
  val make: 'a -> 'a t
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> unit
end

module type STUDENT = INFINITE_ARRAYS

(* A tiny DSL of read and write instructions. *)

type 'a instruction =
  | Read of int * 'a ref  (* index, expected value *)
  | Write of int * 'a     (* index, value *)

type 'a instructions =
  'a instruction list

let default =
  -1

(* Printing an instruction sequence. *)

let sprintf = Printf.sprintf

let print_instruction (i : int instruction) =
  match i with
  | Read (i, xv) ->
      sprintf "let v = get a %d in\n" i ^
      sprintf "if v <> %d then raise Inconsistency;\n" !xv
  | Write (i, v) ->
      sprintf "set a %d %d;\n" i v

let print_instructions is =
  sprintf "let a = make (%d) in\n" default ^
  String.concat "" (map print_instruction is) ^
  "()"

(* [generate n s accu] generates an instruction sequence of length [n]
   where the array indices range between [0] and [s] excluded. The
   generated instructions are prepended in front of [accu]. *)

(* We must keep track of the indices that we have already written, and
   read there with a high likelihood; otherwise we will just read in
   places we have never written, which won't result in a proper test. *)

module IntSet =
  Set.Make(struct type t = int let compare = compare end)

let pick (s : IntSet.t) : int =
  let n = IntSet.cardinal s in
  let i = Random.int n in
  List.nth (IntSet.elements s) i (* careful: very inefficient! *)

let rec generate (n : int) (s : int) (written : IntSet.t) (accu : int instructions) =
  if n = 0 then
    accu
  else
    let instruction, written =
      let i =
        if IntSet.is_empty written || Random.bool() then
          Random.int s
        else
          pick written
      in
      if Random.bool() then
        let dummy = -99 in
        Read (i, ref dummy),
        written
      else
        Write (i, Random.int 100),
        IntSet.add i written
    in
    generate (n-1) s written (instruction :: accu)

let generate n s =
  List.rev (generate n s IntSet.empty [])

let generate n s (accu : int instructions list) =
  generate n s :: accu

let rec repeat n f accu =
  if n = 0 then
    accu
  else
    repeat (n-1) f (f accu)

let annotate (is : int instructions) =
  let b = Solution.make default in
  is |> List.iter (fun i ->
    match i with
    | Read (i, xv) ->
        xv := Solution.get b i;
    | Write (i, v) ->
        Solution.set b i v
  )

let execute student (is : int instructions ref) : unit =
  let module Student = (val student: STUDENT) in
  let a = Student.make default in
  (* Allocate a counter of how many instructions we attempted to
     execute. It is used to truncate the exception sequence when
     an exception occurs. *)
  let c = ref 0 in
  (* Annotate each [get] instruction with its expected result. *)
  annotate !is;
  try
    !is |> List.iter (fun i ->
      incr c;
      match i with
      | Read (i, xv) ->
          let v = Student.get a i in
          if (v <> !xv) then raise Inconsistency
      | Write (i, v) ->
          Student.set a i v
    )
  with e ->
    (* Truncate the instruction sequence. *)
    is := take !c !is;
    raise e

let report_bad_behavior is actual_behavior =
  (* Annotate each [get] instruction with its expected result. *)
  annotate is;
  fail (
    something_is_wrong @
    R.Text "Executing the following instruction sequence:" ::
    R.Break ::
    R.Code (print_instructions is) ::
    R.Break ::
    show_actual_behavior show_unit actual_behavior
  )

let rec shrink student (is : int instructions) actual_behavior =
  let rec loop iss actual_behavior =
    match iss with
    | [] ->
        (* No shrinking step is possible. Stop. *)
        report_bad_behavior is actual_behavior
    | is :: iss ->
        match execute student (ref is) with
        | () ->
            (* Shrinking removed the error. Bad attempt. Loop on. *)
            loop iss actual_behavior
        | exception e ->
            (* Shrinking preserved the error. Commit this shrinking
               step and try to shrink further. *)
            shrink student is (Error e)
  in
  loop (shrink1 is) actual_behavior

let rec power k s accu =
  if k = 0 then
    accu
  else
    (* We must generate short instruction sequences because [pick] is very
       inefficient when we applied to a large set. *)
    let n = 10 in
    let repetitions = 15 in
    repeat repetitions (generate n s) (power (k-1) (2 * s) accu)

let tests : int instructions list =
  power 16 1 []

let test_everything () =
  T.test_student_code [%ty: (module STUDENT)] (fun student ->
    protect (fun () ->
      tests |> List.iter (fun (is : int instructions) ->
        let is = ref is in
        let actual_behavior = T.result (fun () -> execute student is) in
        let is = !is in
        match actual_behavior with
        | Ok () ->
            ()
        | Error TODO ->
            raise TODO
        | Error _ ->
            shrink student is actual_behavior
      );
      success (text "The code seems correct.")
    )
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_everything()

let () =
  T.set_result (T.ast_sanity_check code_ast report)
