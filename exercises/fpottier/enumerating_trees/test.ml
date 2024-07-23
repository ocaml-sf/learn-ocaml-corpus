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

let semicolon =
  char ';'

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

let separate (sep : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] ->
      []
  | x :: xs ->
      x :: List.flatten (List.map (fun x -> [sep; x]) xs)

let concat (docs : document list) : document =
  List.fold_right (^^) docs empty

let comma =
  utf8string "," ^^ break 1

let commas docs =
  concat (separate comma docs)

let semi =
  utf8string ";" ^^ break 1

let semis docs =
  concat (separate semi docs)

let int i =
  utf8format "%d" i

let block doc =
  nest 2 (break 0 ^^ doc) ^^ break 0

let parens doc =
  utf8string "(" ^^ block doc ^^ utf8string ")"

let brackets doc =
  utf8string "[" ^^ block doc ^^ utf8string "]"

let ocaml_array_brackets doc =
  utf8string "[| " ^^ block doc ^^ utf8string "|]"

let tuple docs =
  group (parens (commas docs))

let list docs =
  group (brackets (semis docs))

let construct label docs =
  match docs with
  | [] ->
      utf8string label
  | _ ->
      utf8string label ^^ space ^^ tuple docs

let flow docs =
  match docs with
  | [] ->
      []
  | doc :: docs ->
      doc :: map (fun doc -> group (break 1) ^^ doc) docs

let raw_apply docs =
  group (concat (flow docs))

(* TEMPORARY OLD:
let raw_apply docs =
  group (nest 2 (concat (separate (break 1) docs)))
 *)

let apply f docs =
  raw_apply (utf8string f :: docs)

let wrap (print : 'a -> document) : 'a -> string =
  fun x -> pretty 70 (group (print x))

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

(* When doing black-box testing of a complete module, we are not testing just
   one function in isolation, but a group of functions together. In that case,
   the wording of the error message is somewhat different. Instead of saying
   that a specific function is incorrect, we want to say that an expression
   [expr] yields an incorrect result. *)

let black_box_compare
  (* Value equality and display, used to compare and show results. *)
  eq_value show_value
  (* Expression display. *)
  show_expr expr
  (* Actual behavior and expected behavior. *)
  actual_behavior
  expected_behavior
=
  let success =
    match actual_behavior, expected_behavior with
    | Ok actual, Ok expected ->
        eq_value actual expected (* value comparison *)
    | Error actual, Error expected ->
        actual = expected  (* exception comparison *)
    | Ok _, Error _
    | Error _, Ok _ ->
        false
  in
  (* Allow [TODO] to escape and abort the whole test. *)
  if actual_behavior = Error TODO then
    raise TODO;
  if not success then
    let header = [
      R.Text "Something is wrong.";
      R.Text "The following expression:";
      R.Break;
      R.Code (show_expr expr);
      R.Break;
    ]
    and actual =
      match actual_behavior with
      | Ok actual ->
          R.Text "produces the following value:" ::
          R.Output (show_value actual) ::
          []
      | Error actual ->
          R.Text "raises the following exception:" ::
          R.Output (Printexc.to_string actual) ::
          []
    and expected =
      match expected_behavior with
      | Ok expected ->
          R.Text "Yet, it should produce this value:" ::
          R.Output (show_value expected) ::
          []
      | Error expected ->
          R.Text "Yet, it should raise this exception:" ::
          R.Output (Printexc.to_string expected) ::
          []
    in
    fail (header @ actual @ expected)

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

let print_int =
  int

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

(* A printer for arrays. *)

let print_array print_element a =
  group (ocaml_array_brackets (concat (
    a |> Array.map (fun x ->
      print_element x ^^ semicolon ^^ break 1
    ) |> Array.to_list
  )))

(* A printer for lists. *)

let print_list print_element xs =
  list (map print_element xs)

let print_list_int =
  print_list print_int

let show_list_int =
  wrap print_list_int

(* A printer for homogeneous tuples. *)

let print_tuple print_element xs =
  tuple (map print_element xs)

let print_int_int (x1, x2) =
  print_tuple print_int [x1; x2]

(* -------------------------------------------------------------------------- *)

(* Some code is taken from the solution. *)

open Solution

(* -------------------------------------------------------------------------- *)

(* Expressions that build and inspect enumerations. *)

(* Unfortunately, because we are able to grab the student's code only at a
   monomorphic type, we cannot write a polymorphic evaluator for the type
   ['a expr]. This causes some code duplication further down: in every test,
   we must manually supply 1- an expression; 2- its denotation in the student's
   code; 3- its denotation in the solution. If we had a polymorphic evaluator,
   2- and 3- would be deduced from 1-. *)

type _ expr =
  (* Enumerations. *)
  | EEmpty:    'a enum expr
  | EJust:     'a * ('a -> document) -> 'a enum expr
  | EPay:      'a enum expr -> 'a enum expr
  | ESum:      'a enum expr * 'a enum expr -> 'a enum expr
  | EProduct:  (* balanced: *) bool * 'a enum expr * 'b enum expr -> ('a * 'b) enum expr
  | EMap:      ('a -> 'b) * document * 'a enum expr -> 'b enum expr
  | EBit:      int enum expr
  | EList:     'a enum expr -> 'a list enum expr
  | ETree:     (* balanced: *) bool -> tree enum expr
  | ETidyTree: labeled_tree enum expr
  (* Homemade enumerations. *)
  | EEmptyPos: 'a enum expr
  | ESizedInt: int enum expr
  (* Application of an enumeration to a size [s]. *)
  | EApply:    'a enum expr * int -> 'a Seq.seq expr
  (* Observations of a sequence. *)
  | ELength:   'a Seq.seq expr -> int expr
  | EElements: 'a Seq.seq expr -> 'a list expr
  | ESort:     'a list expr -> 'a list expr

(* An expression printer. *)

let rec print_atomic_expr : type a . a expr -> document =
  fun e ->
    group begin match e with
    | EEmpty ->
        apply "empty" []
    | _ ->
        parens (print_expr e)
    end

and print_expr : type a . a expr -> document =
  fun e ->
    group begin match e with
    | EEmpty ->
        print_atomic_expr e
    | EJust (x, print) ->
        apply "just" [ print x ]
    | EPay e ->
        apply "pay" [ print_atomic_expr e ]
    | ESum (e1, e2) ->
        apply "sum" [ print_atomic_expr e1; print_atomic_expr e2 ]
    | EProduct (false, e1, e2) ->
        apply "product" [ print_atomic_expr e1; print_atomic_expr e2 ]
    | EProduct (true, e1, e2) ->
        apply "balanced_product" [ print_atomic_expr e1; print_atomic_expr e2 ]
    | EMap (f, fs, e) ->
        apply "map" [ fs; print_atomic_expr e ]
    | EBit ->
        utf8string "bit"
    | EList e ->
        apply "list" [ print_atomic_expr e ]
    | ETree false ->
        utf8string "tree"
    | ETree true ->
        utf8string "balanced_tree"
    | ETidyTree ->
        utf8string "balanced_tidy_tree"
    | EEmptyPos ->
        (* A homemade enumeration, which is empty but requires its argument [s]
           to be nonnegative. We use it to detect an incorrect implementation
           of [pay] where the student forgets to test if [s] is zero. *)
        utf8string "fun s ->" ^^ nest 2 (break 1 ^^
          utf8string "if s >= 0 then Seq.empty" ^^ break 1 ^^
          utf8string "else failwith \"A size must be nonnegative.\"")
    | ESizedInt ->
        (* A homemade enumeration, which contains the nonnegative integers [i]
           with size [i]. We use it to test [product]. *)
        utf8string "Seq.singleton"
    | EApply (e, s) ->
        raw_apply [ print_expr e; utf8string "(* applied to size: *)"; int s ]
    | ELength e ->
        apply "Seq.length" [ print_atomic_expr e ]
    | EElements e ->
        apply "Seq.elements" [ print_atomic_expr e ]
    | ESort e ->
        apply "List.sort" [ utf8string "compare"; print_atomic_expr e ]
    end

let show_expr e =
  wrap print_expr e

(* -------------------------------------------------------------------------- *)

(* Printing trees. *)

let rec print_tree (t : tree) =
  group begin match t with
  | Leaf ->
      utf8string "Leaf"
  | Node (t1, t2) ->
      construct "Node" [ print_tree t1; print_tree t2 ]
  end

let rec print_labeled_tree (t : labeled_tree) =
  group begin match t with
  | LLeaf x ->
      construct "LLeaf" [ int x ]
  | LNode (t1, t2) ->
      construct "LNode" [ print_labeled_tree t1; print_labeled_tree t2 ]
  end

(* -------------------------------------------------------------------------- *)

(* Testing an enumeration. *)

(* We test an enumeration by first applying it to a certain size [s], then by
   comparing the resulting sequence against an expected sequence. To compare
   two sequences, we compare them as sets: we sort them and compare them as
   lists. Thus, order is irrelevant. Indeed, order is unspecified in the
   assignment, and in some combinators, such as [product], several different
   orders may naturally appear. *)

let threshold =
  7

let compare_enumerations (type a)
  (print : a -> document) (expr : a enum expr)
  (actual : unit -> a enum) (expected : a enum)
=
  (* Optional: we could compare just the lengths first, before comparing
     actual lists of elements. That would provide more compact messages,
     containing less information. It is probably preferable to aim directly
     for a message that shows the expected sequence, rather than just the
     expected length. *)
  if false then begin
    for s = 0 to threshold do
      (* Compare lengths. *)
      let actual = T.result (fun () -> actual () s |> Seq.length)
      and expected = Ok (expected s |> Seq.length) in
      black_box_compare (=) show_int
        show_expr (ELength (EApply (expr, s)))
        actual expected
    done
  end;
  for s = 0 to threshold do
    (* Compare sorted lists. *)
    let sort = List.sort Stdlib.compare in
    let actual = T.result (fun () -> actual () s |> Seq.elements |> sort)
    and expected = Ok (expected s |> Seq.elements |> sort) in
    black_box_compare (=) (wrap (print_list print))
      show_expr (ESort (EElements (EApply (expr, s))))
      actual expected
  done

let compare_int_enumerations expr actual expected =
  compare_enumerations print_int expr actual expected

let compare_int_int_enumerations expr actual expected =
  compare_enumerations print_int_int expr actual expected

let compare_int_list_enumerations expr actual expected =
  compare_enumerations print_list_int expr actual expected

let compare_tree_enumerations expr actual expected =
  compare_enumerations print_tree expr actual expected

let compare_labeled_tree_enumerations expr actual expected =
  compare_enumerations print_labeled_tree expr actual expected

(* -------------------------------------------------------------------------- *)

(* Empty. *)

let test_empty empty =
  compare_int_enumerations EEmpty (fun () -> empty) Solution.empty;
  correct "empty"

let test_empty () =
  section "Question 1" (
    grab [%ty: int enum] "empty" (fun empty ->
      protect (fun () ->
        test_empty empty
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* Just. *)

let ejust (x : int) =
  EJust (x, print_int)

let test_just just =
  compare_int_enumerations
    (ejust 42)
    (fun () -> just 42)
    (Solution.just 42);
  correct "just"

let test_just () =
  section "Question 2" (
    grab [%ty: int -> int enum] "just" (fun just ->
      protect (fun () ->
        test_just just
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* Pay. *)

(* This is a very limited test. We might wish to test an application of [pay]
   to a more complex enumeration. We could do it, but the problem is, if the
   test fails, we need to be able to show the test that we ran -- thus, the
   enumeration must be built in terms of the tools available so far. At this
   point, we have only [empty] and [just]. *)

let test_pay empty just pay =
  (* Test [pay empty]. *)
  compare_int_enumerations
    (EPay EEmpty)
    (fun () -> pay empty)
    (Solution.(pay empty))
  ;
  (* Test [pay (just 42)]. *)
  compare_int_enumerations
    (EPay (ejust 42))
    (fun () -> pay (just 42))
    (Solution.(pay (just 42)))
  ;
  (* Test [pay (pay (just 42))]. *)
  compare_int_enumerations
    (EPay (EPay (ejust 42)))
    (fun () -> pay (pay (just 42)))
    (Solution.(pay (pay (just 42))))
  ;
  (* Test the application of [pay] to a homemade [empty], which fails when
     it receives a negative size as an argument. This allows us to detect
     a situation where the student forgets to test if [s] is zero. *)
  let empty s =
    if s >= 0 then Seq.empty else failwith "A size must be nonnegative."
  in
  compare_int_enumerations
    (EPay EEmptyPos)
    (fun () -> pay empty)
    (Solution.pay empty)
  ;
  (* Success. *)
  correct "pay"

let test_pay () =
  section "Question 3" (
    grab [%ty: int enum] "empty" (fun empty ->
    grab [%ty: int -> int enum] "just" (fun just ->
    grab [%ty: int enum -> int enum] "pay" (fun pay ->
      protect (fun () ->
        test_pay empty just pay
      )
    )))
  )

(* -------------------------------------------------------------------------- *)

(* Sum. *)

let test_sum empty just pay sum =
  compare_int_enumerations
    (ESum (EEmpty, ejust 42))
    (fun () -> sum empty (just 42))
    (Solution.(sum empty (just 42)))
  ;
  compare_int_enumerations
    (ESum (ejust 42, EEmpty))
    (fun () -> sum (just 42) empty)
    (Solution.(sum (just 42) empty))
  ;
  compare_int_enumerations
    (ESum (ejust 24, ejust 42))
    (fun () -> sum (just 24) (just 42))
    (Solution.(sum (just 24) (just 42)))
  ;
  compare_int_enumerations
    (ESum (ejust 24, EPay (ejust 42)))
    (fun () -> sum (just 24) (pay (just 42)))
    (Solution.(sum (just 24) (pay (just 42))))
  ;
  (* Success. *)
  correct "sum"

let test_sum () =
  section "Question 4" (
    grab [%ty: int enum] "empty" (fun empty ->
    grab [%ty: int -> int enum] "just" (fun just ->
    grab [%ty: int enum -> int enum] "pay" (fun pay ->
    grab [%ty: int enum -> int enum -> int enum] "sum" (fun sum ->
      protect (fun () ->
        test_sum empty just pay sum
      )
    ))))
  )

(* -------------------------------------------------------------------------- *)

(* Product. *)

let sized_int =
  Seq.singleton

let test_product empty just pay product =
  (* [product empty _] and [product _ empty]. *)
  compare_int_int_enumerations
    (EProduct (false, EEmpty, ejust 42))
    (fun () -> product empty (just 42))
    (Solution.(product empty (just 42)))
  ;
  compare_int_int_enumerations
    (EProduct (false, ejust 42, EEmpty))
    (fun () -> product (just 42) empty)
    (Solution.(product (just 42) empty))
  ;
  compare_int_int_enumerations
    (EProduct (false, EEmpty, ESizedInt))
    (fun () -> product empty sized_int)
    (Solution.(product empty sized_int))
  ;
  compare_int_int_enumerations
    (EProduct (false, ESizedInt, EEmpty))
    (fun () -> product sized_int empty)
    (Solution.(product sized_int empty))
  ;
  (* Products involving [just _] and [pay]. *)
  compare_int_int_enumerations
    (EProduct (false, ejust 24, ejust 42))
    (fun () -> product (just 24) (just 42))
    (Solution.(product (just 24) (just 42)))
  ;
  compare_int_int_enumerations
    (EProduct (false, EPay (ejust 24), EPay (ejust 42)))
    (fun () -> product (pay (just 24)) (pay (just 42)))
    (Solution.(product (pay (just 24)) (pay (just 42))))
  ;
  (* A product of two enumerations of the integers, where the integer [i]
     has size [i]. This yields an enumeration where a pair [(i, j)] appears
     at level [s] if and only if [i + j] is [s]. *)
  compare_int_int_enumerations
    (EProduct (false, ESizedInt, ESizedInt))
    (fun () -> product sized_int sized_int)
    (Solution.(product sized_int sized_int))
  ;
  (* Success. *)
  correct "product"

let test_product () =
  section "Question 5" (
    grab [%ty: int enum] "empty" (fun empty ->
    grab [%ty: int -> int enum] "just" (fun just ->
    grab [%ty: int enum -> int enum] "pay" (fun pay ->
    grab [%ty: int enum -> int enum -> (int * int) enum] "product" (fun product ->
      protect (fun () ->
        test_product empty just pay product
      )
    ))))
  )

(* -------------------------------------------------------------------------- *)

(* Map. *)

let emap_succ e =
  EMap (succ, utf8string "succ", e)

let test_map empty just pay map =
  compare_int_enumerations
    (emap_succ EEmpty)
    (fun () -> map succ empty)
    (Solution.(map succ empty))
  ;
  compare_int_enumerations
    (emap_succ (ejust 42))
    (fun () -> map succ (just 42))
    (Solution.(map succ (just 42)))
  ;
  compare_int_enumerations
    (emap_succ (EPay (ejust 42)))
    (fun () -> map succ (pay (just 42)))
    (Solution.(map succ (pay (just 42))))
  ;
  compare_int_enumerations
    (emap_succ ESizedInt)
    (fun () -> map succ sized_int)
    (Solution.(map succ sized_int))
  ;
  (* Success. *)
  correct "map"

let test_map () =
  section "Question 6" (
    grab [%ty: int enum] "empty" (fun empty ->
    grab [%ty: int -> int enum] "just" (fun just ->
    grab [%ty: int enum -> int enum] "pay" (fun pay ->
    grab [%ty: (int -> int) -> int enum -> int enum] "map" (fun map ->
      protect (fun () ->
        test_map empty just pay map
      )
    ))))
  )

(* -------------------------------------------------------------------------- *)

(* Bit. *)

let test_bit bit =
  compare_int_enumerations EBit (fun () -> bit) Solution.bit;
  correct "bit"

let test_bit () =
  section "Question 7" (
    grab [%ty: int enum] "bit" (fun bit ->
      protect (fun () ->
        test_bit bit
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* List. *)

let test_list bit list =
  compare_int_list_enumerations
    (EList EBit)
    (fun () -> list bit)
    (Solution.(list bit))
  ;
  correct "list"

let test_list () =
  section "Question 8" (
    grab [%ty: int enum] "bit" (fun bit ->
    grab [%ty: int enum -> int list enum] "list" (fun list ->
      protect (fun () ->
        test_list bit list
      )
    ))
  )

(* -------------------------------------------------------------------------- *)

(* Tree. *)

let test_tree tree =
  compare_tree_enumerations
    (ETree false)
    (fun () -> tree)
    (Solution.tree)
  ;
  correct "tree"

let test_tree () =
  section "Question 9" (
    grab [%ty: tree enum] "tree" (fun tree ->
      protect (fun () ->
        test_tree tree
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* Balanced product. *)

let test_balanced_product empty just pay balanced_product =
  (* [balanced_product empty _] and [balanced_product _ empty]. *)
  compare_int_int_enumerations
    (EProduct (true, EEmpty, ejust 42))
    (fun () -> balanced_product empty (just 42))
    (Solution.(balanced_product empty (just 42)))
  ;
  compare_int_int_enumerations
    (EProduct (true, ejust 42, EEmpty))
    (fun () -> balanced_product (just 42) empty)
    (Solution.(balanced_product (just 42) empty))
  ;
  compare_int_int_enumerations
    (EProduct (true, EEmpty, ESizedInt))
    (fun () -> balanced_product empty sized_int)
    (Solution.(balanced_product empty sized_int))
  ;
  compare_int_int_enumerations
    (EProduct (true, ESizedInt, EEmpty))
    (fun () -> balanced_product sized_int empty)
    (Solution.(balanced_product sized_int empty))
  ;
  (* Products involving [just _] and [pay]. *)
  compare_int_int_enumerations
    (EProduct (true, ejust 24, ejust 42))
    (fun () -> balanced_product (just 24) (just 42))
    (Solution.(balanced_product (just 24) (just 42)))
  ;
  compare_int_int_enumerations
    (EProduct (true, EPay (ejust 24), EPay (ejust 42)))
    (fun () -> balanced_product (pay (just 24)) (pay (just 42)))
    (Solution.(balanced_product (pay (just 24)) (pay (just 42))))
  ;
  (* A product of two enumerations of the integers, where the integer [i]
     has size [i]. This yields an enumeration where a pair [(i, j)] appears
     at level [s] if and only if [i + j] is [s] and [i], [j] differ by at
     most one. *)
  compare_int_int_enumerations
    (EProduct (true, ESizedInt, ESizedInt))
    (fun () -> balanced_product sized_int sized_int)
    (Solution.(balanced_product sized_int sized_int))
  ;
  (* Success. *)
  correct "balanced_product"

let test_balanced_product () =
  section "Question 10" (
    grab [%ty: int enum] "empty" (fun empty ->
    grab [%ty: int -> int enum] "just" (fun just ->
    grab [%ty: int enum -> int enum] "pay" (fun pay ->
    grab [%ty: int enum -> int enum -> (int * int) enum]
      "balanced_product" (fun balanced_product ->
      protect (fun () ->
        test_balanced_product empty just pay balanced_product
      )
    ))))
  )

(* -------------------------------------------------------------------------- *)

(* Balanced tree. *)

let test_balanced_tree balanced_tree =
  compare_tree_enumerations
    (ETree true)
    (fun () -> balanced_tree)
    (Solution.balanced_tree)
  ;
  correct "balanced_tree"

let test_balanced_tree () =
  section "Question 11" (
    grab [%ty: tree enum] "balanced_tree" (fun balanced_tree ->
      protect (fun () ->
        test_balanced_tree balanced_tree
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* Balanced tidy tree. *)

let test_balanced_tidy_tree balanced_tidy_tree =
  compare_labeled_tree_enumerations
    (ETidyTree)
    (fun () -> balanced_tidy_tree)
    (Solution.balanced_tidy_tree)
  ;
  correct "balanced_tidy_tree"

let test_balanced_tidy_tree () =
  section "Question 12" (
    grab [%ty: labeled_tree enum] "balanced_tidy_tree" (fun balanced_tidy_tree ->
      protect (fun () ->
        test_balanced_tidy_tree balanced_tidy_tree
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_empty() @
  test_just() @
  test_pay() @
  test_sum() @
  test_product() @
  test_map() @
  test_bit() @
  test_list() @
  test_tree() @
  test_balanced_product() @
  test_balanced_tree() @
  test_balanced_tidy_tree() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
