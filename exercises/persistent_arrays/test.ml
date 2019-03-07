open Printf
let iter = List.iter
let map = List.map
module T = Test_lib
module R = Report
type report = R.t

(* The auto-grader. *)

(* -------------------------------------------------------------------------- *)

(* Some of the code below should move to separate library files. *)

(* -------------------------------------------------------------------------- *)

(* PPrintMini. *)

(* -------------------------------------------------------------------------- *)

(* A type of integers with infinity. *)

type requirement =
    int (* with infinity *)

(* Infinity is encoded as [max_int]. *)

let infinity : requirement =
  max_int

(* Addition of integers with infinity. *)

let (++) (x : requirement) (y : requirement) : requirement =
  if x = infinity || y = infinity then
    infinity
  else
    x + y

(* Comparison between an integer with infinity and a normal integer. *)

let (<==) (x : requirement) (y : int) =
  x <= y

(* -------------------------------------------------------------------------- *)

(* The type of documents. See [PPrintEngine] for documentation. *)

type document =
  | Empty
  | FancyString of string * int * int * int
  | Blank of int
  | IfFlat of document * document
  | HardLine
  | Cat of requirement * document * document
  | Nest of requirement * int * document
  | Group of requirement * document

(* -------------------------------------------------------------------------- *)

(* Retrieving or computing the space requirement of a document. *)

let rec requirement = function
  | Empty ->
      0
  | FancyString (_, _, _, len)
  | Blank len ->
      len
  | IfFlat (doc1, _) ->
      requirement doc1
  | HardLine ->
      infinity
  | Cat (req, _, _)
  | Nest (req, _, _)
  | Group (req, _) ->
      req

(* -------------------------------------------------------------------------- *)

(* Document constructors. *)

let empty =
  Empty

let fancysubstring s ofs len apparent_length =
  if len = 0 then
    empty
  else
    FancyString (s, ofs, len, apparent_length)

let fancystring s apparent_length =
  fancysubstring s 0 (String.length s) apparent_length

let utf8_length s =
  let rec length_aux s c i =
    if i >= String.length s then c else
    let n = Char.code (String.unsafe_get s i) in
    let k =
      if n < 0x80 then 1 else
      if n < 0xe0 then 2 else
      if n < 0xf0 then 3 else 4
    in
    length_aux s (c + 1) (i + k)
  in
  length_aux s 0 0

let utf8string s =
  fancystring s (utf8_length s)

let utf8format f =
  ksprintf utf8string f

let char c =
  assert (c <> '\n');
  fancystring (String.make 1 c) 1

let space =
  char ' '

let hardline =
  HardLine

let blank n =
  match n with
  | 0 ->
      empty
  | 1 ->
      space
  | _ ->
      Blank n

let ifflat doc1 doc2 =
  match doc1 with
  | IfFlat (doc1, _)
  | doc1 ->
      IfFlat (doc1, doc2)

let internal_break i =
  ifflat (blank i) hardline

let break0 =
  internal_break 0

let break1 =
  internal_break 1

let break i =
  match i with
  | 0 ->
      break0
  | 1 ->
      break1
  | _ ->
      internal_break i

let (^^) x y =
  match x, y with
  | Empty, _ ->
      y
  | _, Empty ->
      x
  | _, _ ->
      Cat (requirement x ++ requirement y, x, y)

let nest i x =
  assert (i >= 0);
  Nest (requirement x, i, x)

let group x =
  let req = requirement x in
  if req = infinity then
    x
  else
    Group (req, x)

(* -------------------------------------------------------------------------- *)

(* Printing blank space (indentation characters). *)

let blank_length =
  80

let blank_buffer =
  String.make blank_length ' '

let rec blanks output n =
  if n <= 0 then
    ()
  else if n <= blank_length then
    Buffer.add_substring output blank_buffer 0 n
  else begin
    Buffer.add_substring output blank_buffer 0 blank_length;
    blanks output (n - blank_length)
  end

(* -------------------------------------------------------------------------- *)

(* The rendering engine maintains the following internal state. *)

(* For simplicity, the ribbon width is considered equal to the line
   width; in other words, there is no ribbon width constraint. *)

(* For simplicity, the output channel is required to be an OCaml buffer.
   It is stored within the [state] record. *)

type state =
  {
    (* The line width. *)
    width: int;
    (* The current column. *)
    mutable column: int;
    (* The output buffer. *)
    mutable output: Buffer.t;
  }

(* -------------------------------------------------------------------------- *)

(* For simplicity, the rendering engine is *not* in tail-recursive style. *)

let rec pretty state (indent : int) (flatten : bool) doc =
  match doc with

  | Empty ->
      ()

  | FancyString (s, ofs, len, apparent_length) ->
      Buffer.add_substring state.output s ofs len;
      state.column <- state.column + apparent_length

  | Blank n ->
      blanks state.output n;
      state.column <- state.column + n

  | HardLine ->
      assert (not flatten);
      Buffer.add_char state.output '\n';
      blanks state.output indent;
      state.column <- indent

  | IfFlat (doc1, doc2) ->
      pretty state indent flatten (if flatten then doc1 else doc2)

  | Cat (_, doc1, doc2) ->
      pretty state indent flatten doc1;
      pretty state indent flatten doc2

  | Nest (_, j, doc) ->
      pretty state (indent + j) flatten doc

  | Group (req, doc) ->
      let flatten = flatten || state.column ++ req <== state.width in
      pretty state indent flatten doc

(* -------------------------------------------------------------------------- *)

(* The engine's entry point. *)

let pretty width doc =
  let output = Buffer.create 512 in
  let state = { width; column = 0; output } in
  pretty state 0 false doc;
  Buffer.contents output

(* -------------------------------------------------------------------------- *)

(* Additions to PPrintMini. *)

let concat (docs : document list) : document =
  List.fold_right (^^) docs empty

let comma =
  utf8string "," ^^ break 1

let pre_comma doc =
  comma ^^ doc

let pre_commas docs =
  concat (map pre_comma docs)

let commas docs =
  match docs with
  | [] ->
      empty
  | doc :: docs ->
      doc ^^ pre_commas docs

let int i =
  utf8format "%d" i

let parens doc =
  utf8string "(" ^^ doc ^^ utf8string ")"

let parens doc =
  parens (nest 2 (break 0 ^^ doc) ^^ break 0)

let tuple docs =
  group (parens (commas docs))

let construct label docs =
  utf8string label ^^ space ^^ tuple docs

let wrap (print : 'a -> document) : 'a -> string =
  fun x -> pretty 40 (group (print x))

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

(* [successful] tests whether a report is successful. *)

let successful_status = function
  | R.Success _
  | R.Warning
  | R.Informative
  | R.Important ->
     true
  | R.Failure ->
     false

let rec successful_item = function
  | R.Section (_, r) ->
      successful r
  | R.Message (_, status) ->
      successful_status status

and successful (r : report) =
  List.for_all successful_item r

let (-@>) (r : report) (f : unit -> report) : report =
  if successful r then
    r @ f()
  else
    r

(* -------------------------------------------------------------------------- *)

(* Generic test functions. *)

let grab ty name k =
  T.test_value (T.lookup_student ty name) k

let test_value_0 name ty reference eq =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      if not (eq candidate reference) then
        fail [
          R.Code name; R.Text "is incorrect.";
        ];
      let message = [ R.Code name; R.Text "is correct."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  )

let correct name =
  let message = [ R.Code name; R.Text "seems correct."; ] in
  [ R.Message (message, R.Success 1) ]

let mismatch name x actual expected n displayx showy =
  fail (
    R.Code name :: R.Text "is incorrect." ::
    R.Break ::
    let plural = if n > 1 then "s" else "" in
    R.Text (sprintf "When applied to the following argument%s:" plural) ::
    R.Break ::
    displayx x @
    R.Text "it produces the following invalid result:" ::
    R.Break ::
    R.Output (showy actual) ::
    R.Text "A valid result is:" ::
    R.Break ::
    R.Output (showy expected) ::
    []
  )

let codebreak show x =
  [ R.Code (show x); R.Break ]

let test_value_1 name ty reference showx showy eqy tests =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun x ->
        let actual = candidate x
        and expected = reference x in
        if not (eqy actual expected) then
          mismatch name x actual expected
            1 (codebreak showx)
            showy
      );
      correct name
    )
  )

let test_value_2 name ty reference showx1 showx2 showy eqy tests =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2) as x) ->
        let actual = candidate x1 x2
        and expected = reference x1 x2 in
        if not (eqy actual expected) then
          mismatch name x actual expected
            2 (fun (x1, x2) -> codebreak showx1 x1 @ codebreak showx2 x2)
            showy
      );
      correct name
    )
  )

let test_value_3 name ty reference showx1 showx2 showx3 showy eqy tests =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2, x3) as x) ->
        let actual = candidate x1 x2 x3
        and expected = reference x1 x2 x3 in
        if not (eqy actual expected) then
          mismatch name x actual expected
            3 (fun (x1, x2, x3) ->
              codebreak showx1 x1 @ codebreak showx2 x2 @ codebreak showx3 x3)
            showy
      );
      correct name
    )
  )

(* -------------------------------------------------------------------------- *)

(* List-based enumerations. *)

let flat_map f xss =
  List.flatten (List.map f xss)

(* [up i j] is the list of the integers of [i] included up to [j] excluded. *)

(* [upk i j k] is [up i j @ k]. *)

let rec upk i j k =
  if i < j then
    i :: upk (i + 1) j k
  else
    k

let up i j =
  upk i j []

(* [pairs xs ys] is the list of all pairs [x, y] where [x] is drawn from [xs]
   and [y] is drawn from [ys]. In other words, it is the Cartesian product of
   the lists [xs] and [ys]. *)

let pairs xs ys =
  xs |> flat_map (fun x ->
    ys |> flat_map (fun y ->
      [x, y]
    )
  )

(* [split n f] enumerates all manners of splitting [n] into [n1 + n2], where
   [n1] and [n2] can be zero. For each such split, the enumeration [f n1 n2]
   is produced. *)

let split n f =
  flat_map (fun n1 ->
    let n2 = n - n1 in
    f n1 n2
  ) (up 0 (n+1))

(* If [f i] is an enumeration, then [deepening f n] is the concatenation
   of the enumerations [f 0, f 1, ... f n]. *)

let deepening (f : int -> 'a list) (n : int) : 'a list =
  flat_map f (up 0 (n+1))

(* -------------------------------------------------------------------------- *)

(* Printers. *)

(* A printer for strings. *)

let show_string s =
  sprintf "\"%s\"" (String.escaped s)

(* A printer for integers. *)

let show_int i =
  sprintf "%d" i

(* A printer for characters. *)

let show_char c =
  sprintf "'%s'" (Char.escaped c)

(* A printer for Booleans. *)

let show_bool b =
  if b then "true" else "false"

(* A printer for options. *)

let print_option print = function
  | None ->
       utf8string "None"
  | Some x ->
       construct "Some" [ print x ]

(* -------------------------------------------------------------------------- *)

(* Some code is taken from the solution. *)

open Solution

(* -------------------------------------------------------------------------- *)

(* White-box testing. *)

(* The data structure can become ill-formed (that is, cyclic) if the student
   writes incorrect code. Better detect this. *)

exception Cycle

let rec wf visited a =
  match a.contents with
  | Array _ ->
      ()
  | Apply { base; _ } ->
      if (List.memq base visited) then raise Cycle;
      wf (a :: visited) base

let wf a =
  wf [] a

(* Following a path up to its end allows us to check that the changes that we
   expect in the data structure are indeed taking place. *)

let rec representative a =
  match a.contents with
  | Array _ ->
      a
  | Apply { base; _ } ->
      representative base

let rec distance a =
  match a.contents with
  | Array _ ->
      0
  | Apply { base; _ } ->
      1 + distance base

(* -------------------------------------------------------------------------- *)

(* Black-box testing. *)

(* A DSL for test scenarios. *)

type var =
  int

type index =
  int

type value =
  bool

type observation =
  value

type term =
  | ELetMake of (* size: *) int * value * term
  | ESeqGet of observation option * var * index * term
  | ELetSet of var * index * value * term
  | EDone

(* A static environment [senv] counts how many variables are in scope. *)

type senv =
  int

let sempty : senv =
  0

(* A term printer, in OCaml syntax. *)

let show_var senv x =
  (* Convert de Bruijn index to de Bruijn level, then apply a fixed mapping of de
     Bruijn levels to strings. We use the names 'a' to 'z'. *)
  let x = senv - x - 1 in
  assert (0 <= x);
  assert (x < 26);
  let c = Char.chr (Char.code 'a' + x) in
  String.make 1 c

let rec print_term senv t =
  match t with
  | ELetMake (size, v, u) ->
      let senv', x' = senv + 1, 0 in
      utf8format "let %s = make %d %b;;" (show_var senv' x') size v ^^
      break_print_term senv' u
  | ESeqGet (oo, x, i, u) ->
      utf8format "let observed = get %s %d;;" (show_var senv x) i ^^
      begin match oo with
      | None ->
          empty
      | Some expected ->
          break 1 ^^
          utf8format "assert (observed = %b);;" expected
      end ^^
      break_print_term senv u
  | ELetSet (x, i, v, u) ->
      let senv', x' = senv + 1, 0 in
      utf8format "let %s = set %s %d %b;;"
        (show_var senv' x') (show_var senv x) i v ^^
      break_print_term senv' u
  | EDone ->
      empty

and break_print_term senv u =
  match u with
  | EDone ->
      empty
  | _ ->
      break 1 ^^ print_term senv u

let show_term =
  wrap (print_term sempty)

(* Enumerating terms. *)

(* [size] is the size of the persistent arrays that we should create.
   [senv] indicates how many variables are in scope.
   [n] is the size of the terms that we should enumerate.
   [m] is the number of calls to [make] that are still permitted. *)

let max_vars =
  4

let default_value =
  false

let rec terms senv m size n : term list =
  if n = 0 then
    [EDone]
  else
    let items1 =
      if senv < max_vars && 0 < m then
        (* EMake. *)
        terms (senv+1) (m-1) size (n-1) |> flat_map (fun u ->
          [ELetMake (size, default_value, u)]
        )
      else
        []
    in
    let items2 =
      (* ESeqGet. *)
      up 0 senv |> flat_map (fun x ->
        up 0 size |> flat_map (fun i ->
          terms senv m size (n-1) |> flat_map (fun u ->
            [ESeqGet (None, x, i, u)]
          )
        )
      )
    in
    let items3 =
      if senv < max_vars then
        (* ELetSet. *)
        up 0 senv |> flat_map (fun x ->
          up 0 size |> flat_map (fun i ->
            terms (senv+1) m size (n-1) |> flat_map (fun u ->
              [true; false] |> flat_map (fun v ->
                [ELetSet (x, i, v, u)]
              )
            )
          )
        )
      else
        []
    in
    items1 @ items2 @ items3

let terms =
  terms sempty

(* Interpreting terms. *)

let lookup =
  List.nth

module Interpret (I : sig
  val make: int -> value -> value parray
  val get: value parray -> int -> value
  val set: value parray -> int -> value -> value parray
end) = struct

  type env =
    value parray list

  let empty : env =
    []

  (* The interpreter transforms an unannotated term into a term that is
     annotated with observations. If applied to a term that is annotated
     already with observations, then it regards these observations as expected
     results and checks that the actual results match the expected results. *)

  (* At every step, the interpreter checks that every persistent array is
     well-formed. Thus, if the student creates a cycle, we detect it early. *)

  exception Check

  let check oo (actual : value) =
    match oo with
    | Some expected when actual <> expected -> raise Check
    | _ -> ()

  let rec interpret env t =
    match t with
    | ELetMake (size, v, u) ->
        let env = I.make size v :: env in
        iter wf env;
        let u = interpret env u in
        ELetMake (size, v, u)
    | ESeqGet (oo, x, i, u) ->
        let actual = I.get (lookup env x) i in
        check oo actual;
        iter wf env;
        let u = interpret env u in
        ESeqGet (Some actual, x, i, u)
    | ELetSet (x, i, v, u) ->
        let env = I.set (lookup env x) i v :: env in
        iter wf env;
        let u = interpret env u in
        ELetSet (x, i, v, u)
    | EDone ->
        EDone

  let interpret : term -> term =
    interpret empty

end

(* -------------------------------------------------------------------------- *)

(* Testing for correctness. *)

(* This is purely black-box testing, except for a well-formedness check that
   is performed on the fly so as to produce a better report if the graph
   becomes ill-formed. *)

let tests =
  (* Allow max one call to [make]. I would like to allow two, but it is a
     bit slower, and does not make a difference in practice, I think. *)
  let m = 1 in
  (* Array size 0, term size 1. *)
  terms m 0 1 @
  (* Array size 1, term size up to 6. *)
  deepening (terms m 1) 6 @
  (* Array size 2, term size up to 4. *)
  deepening (terms m 2) 4

let test_correctness make get set =
  let module Reference = Interpret(Solution) in
  let module Candidate = Interpret(struct
    let make = make
    let get = get
    let set = set
  end) in
  tests |> iter (fun (scenario : term) ->
    (* Annotate the terms with expected results. *)
    let scenario = Reference.interpret scenario in
    (* Check that the candidate implementation meets our expectations. *)
    try
      let _ = Candidate.interpret scenario in ()
    with
    | Candidate.Check ->
        fail [
          R.Text "The code is incorrect.";
          R.Text "In the following scenario, an assertion is violated:";
          R.Break;
          R.Code (show_term scenario);
        ]
    | Cycle ->
        fail [
          R.Text "The code is incorrect.";
          R.Text "In the following scenario, the graph becomes cyclic:";
          R.Break;
          R.Code (show_term scenario);
        ]
    | TODO ->
        raise TODO
    | (e : exn) ->
        fail [
          R.Text "The code is incorrect.";
          R.Text "In the following scenario:";
          R.Break;
          R.Code (show_term scenario);
          R.Text "The following exception is raised and never caught:";
          R.Break;
          R.Output (Printexc.to_string e);
        ]
  );
  let message = [
    R.Text "The code seems correct.";
    R.Text (sprintf "It passes %d test scenarios." (List.length tests));
  ] in
  [ R.Message (message, R.Success 1) ]

let test_correctness () =
  section "Correctness" (
    grab [%ty : int -> value -> value parray] "make" (fun make ->
    grab [%ty : value parray -> int -> value] "get" (fun get ->
    grab [%ty : value parray -> int -> value -> value parray] "set" (fun set ->
    protect (fun () ->
      test_correctness make get set
    ))))
  )

(* -------------------------------------------------------------------------- *)

(* Testing for obedience to the optimised scheme. *)

(* In this test, we create a single base persistent array and perform a series
   of [get] and [set] operations. After each operation, we check that the
   representative of every persistent array is as expected. *)

(* This is a form of white-box testing. *)

let check_representatives scenario env r =
  (* Check that every persistent array in [env] has representative [r]. *)
  let senv = List.length env in
  let envr = lookup env r in
  env |> List.iteri (fun j envj ->
    if representative envj != envr then
      fail [
        R.Text "The optimization scheme of Question 4 is not respected.";
        R.Text "At the end of the following scenario:";
        R.Break;
        R.Code (show_term scenario);
        R.Text "The graph should have a path from";
        R.Code (show_var senv j);
        R.Text "to";
        R.Code (show_var senv r);
        R.Text "and no further.";
        R.Text "Yet, this is not the case.";
      ]
  )

let play make get set scenario =
  let full_scenario = scenario in
  match scenario with
  | ESeqGet _
  | ELetSet _ ->
      (* This should not happen; every scenario begins with [make]. *)
      assert false
  | EDone ->
      ()
  | ELetMake (size, v, scenario) ->
      (* Interpret the initial call to [make]. Allocate the environment. *)
      let env = [make size v] in
      let r = 0 in
      (* [r] is the current representative of every persistent array in [env]. *)
      let rec loop r scenario env =
        match scenario with
        | ELetMake _ ->
            (* This should not happen; a scenario has at most one [make]. *)
            assert false
        | EDone ->
            ()
        | ESeqGet (_, x, index, scenario) ->
            let _ : value = get (lookup env x) index in
            (* The representative should now be [x]. *)
            let r = x in
            check_representatives full_scenario env r;
            loop r scenario env
        | ELetSet (x, index, v, scenario) ->
            let a = set (lookup env x) index v in
            (* The representative does not change. *)
            let r, env = r + 1, a :: env in
            check_representatives full_scenario env r;
            loop r scenario env
      in
      loop r scenario env

let test_optimization () =
  let tests =
    (* Allow only one call to [make] in a test scenario. *)
    let m = 1 in
    (* Array size 1, term size up to 6. *)
    deepening (terms m 1) 6 @
    (* Array size 2, term size up to 4. *)
    deepening (terms m 2) 4
  in
  section "Optimization" (
    grab [%ty : int -> value -> value parray] "make" (fun make ->
    grab [%ty : value parray -> int -> value] "get" (fun get ->
    grab [%ty : value parray -> int -> value -> value parray] "set" (fun set ->
    protect (fun () ->
      tests |> iter (fun scenario ->
        play make get set scenario
      );
      let message = [
        R.Text "The code seems to respect the optimization scheme of Question 4.";
      ] in
      [ R.Message (message, R.Success 1) ]
    ))))
  )

(* -------------------------------------------------------------------------- *)

(* Testing complexity. *)

(* If the above test is satisfied, chances are complexity is good as well,
   but let's check anyway. *)

let check_cost scenario actual_cost expected_cost =
  if actual_cost > expected_cost then
    fail [
      R.Text "The desired complexity is not achieved.";
      R.Text "In the following scenario:";
      R.Break;
      R.Code (show_term scenario);
      (* We hope that the problematic call is indeed the last call.
         This should be the case, as we use iterative deepening. *)
      R.Text (sprintf
        "The last call involves %d uses of \
         dereferencing (!), whereas %d uses should be enough."
        actual_cost expected_cost
      );
    ]

let play make get set scenario =
  let full_scenario = scenario in
  match scenario with
  | ESeqGet _
  | ELetSet _ ->
      (* This should not happen; every scenario begins with [make]. *)
      assert false
  | EDone ->
      ()
  | ELetMake (size, v, scenario) ->
      (* Interpret the initial call to [make]. Allocate the environment. *)
      let env = [make size v] in
      let rec loop scenario env =
        match scenario with
        | ELetMake _ ->
            (* This should not happen; a scenario has at most one [make]. *)
            assert false
        | EDone ->
            ()
        | ESeqGet (_, x, index, scenario) ->
            let d = distance (lookup env x) in
            let cost = get_count() in
            let _ : value = get (lookup env x) index in
            let cost = get_count() - cost in
            check_cost full_scenario cost (d+1);
            loop scenario env
        | ELetSet (x, index, v, scenario) ->
            let d = distance (lookup env x) in
            let cost = get_count() in
            let a = set (lookup env x) index v in
            let cost = get_count() - cost in
            check_cost full_scenario cost 0;
            let env = a :: env in
            loop scenario env
      in
      loop scenario env

let test_complexity () =
  let tests =
    (* Allow only one call to [make] in a test scenario. *)
    let m = 1 in
    (* Array size 1, term size up to 6. *)
    deepening (terms m 1) 6
  in
  section "Complexity" (
    grab [%ty : int -> value -> value parray] "make" (fun make ->
    grab [%ty : value parray -> int -> value] "get" (fun get ->
    grab [%ty : value parray -> int -> value -> value parray] "set" (fun set ->
    protect (fun () ->
      tests |> iter (fun scenario ->
        play make get set scenario
      );
      let message = [
        R.Text "The code seems to have the desired complexity.";
      ] in
      [ R.Message (message, R.Success 1) ]
    ))))
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_correctness() -@>
  test_optimization -@>
  test_complexity

let () =
  T.set_result (T.ast_sanity_check code_ast report)
