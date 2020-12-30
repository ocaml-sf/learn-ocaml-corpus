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

let block doc =
  nest 2 (break 0 ^^ doc) ^^ break 0

let parens doc =
  utf8string "(" ^^ block doc ^^ utf8string ")"

let ocaml_array_brackets doc =
  utf8string "[| " ^^ block doc ^^ utf8string "|]"

let tuple docs =
  group (parens (commas docs))

let construct label docs =
  utf8string label ^^ space ^^ tuple docs

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

(* A printer for arrays. *)

let print_array print_element a =
  group (ocaml_array_brackets (concat (
    a |> Array.map (fun x ->
      print_element x ^^ semicolon ^^ break 1
    ) |> Array.to_list
  )))

(* -------------------------------------------------------------------------- *)

(* Some code is taken from the solution. *)

open Solution

(* -------------------------------------------------------------------------- *)

(* Enumerating trees. *)

let list_trees_of_weight =
  fix (fun list_trees_of_weight w ->
    if w = 0 then
      [Leaf]
    else
      split (w - 1) (fun w1 w2 ->
        list_trees_of_weight w1 |> flat_map (fun t1 ->
          list_trees_of_weight w2 |> flat_map (fun t2 ->
            [Node (t1, t2)]
          )
        )
      )
  )

(* -------------------------------------------------------------------------- *)

(* Printing trees. *)

let rec print_tree (t : tree) =
  group begin match t with
  | Leaf ->
      utf8string "Leaf"
  | Node (t1, t2) ->
      construct "Node" [ print_tree t1; print_tree t2 ]
  end

let show_tree =
  wrap print_tree

(* Printing arrays. *)

let show_int_array =
  wrap (print_array int)

(* -------------------------------------------------------------------------- *)

(* Weight and height. *)

let tests : tree list =
  deepening list_trees_of_weight 10

let test_weight () =
  section "Question 1" (
    test_value_1 "weight" [%ty: tree -> int] weight
      show_tree show_int (=) tests
  )

let test_height () =
  section "Question 2" (
    test_value_1 "height" [%ty: tree -> int] height
      show_tree show_int (=) tests
  )

(* -------------------------------------------------------------------------- *)

(* Naive tree count. *)

let test_naive_trees_of_weight () =
  section "Question 3" (
    test_value_1 "naive_trees_of_weight" [%ty: int -> int] trees_of_weight
      show_int show_int (=) (up 0 10)
  )

(* -------------------------------------------------------------------------- *)

(* Dynamic programming. *)

let test_trees_of_weights () =
  section "Question 4" (
    test_value_1 "trees_of_weights" [%ty: int -> int array] trees_of_weights
      show_int show_int_array (=) [0; 1; 2; 5; 10; 15; 20]
  )

(* -------------------------------------------------------------------------- *)

(* Memoization. *)

(* We test [fix] for correctness and complexity. We use a customised version
   of [test_value_1], because [fix] is a higher-order function and because
   measuring complexity requires hacks. *)

let c =
  ref 0

let mismatch_cost name x actual_cost expected_cost displayx =
  fail (
    R.Code name :: R.Text "has incorrect complexity." ::
    R.Break ::
    R.Text "When applied to the following argument:" ::
    R.Break ::
    displayx x @
    R.Text (sprintf "%d calls are performed, whereas %d should suffice."
            actual_cost expected_cost) ::
    []
  )

let tests =
  [
    (fun fact n -> incr c; if n = 0 then 1 else n * fact(n-1)),
    "(fun fact n -> if n = 0 then 1 else n * fact(n-1))",
    [10]
      ;
    (fun fib n -> incr c; if n <= 1 then 1 else fib(n-2) + fib(n-1)),
    "(fun fib n -> if n <= 1 then 1 else fib(n-2) + fib(n-1))",
    [10]
  ]

let test_fix () =
  section "Question 5" (
    (* A customised [test_value_1]. *)
    let name = "fix"
    and ty = [%ty: (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)]
    and reference = fix
    and showy, eqy = show_int, (=) in
    T.test_value (T.lookup_student ty name) (fun candidate ->
      protect (fun () ->
        (* Functional correctness. *)
        tests |> iter (fun (ff, desc, xs) ->
          let candidate = candidate ff
          and reference = reference ff in
          xs |> iter (fun x ->
            let actual = candidate x in
            let expected = reference x in
            if not (eqy actual expected) then
              let showx x = sprintf "%s, %d" desc x in
              mismatch name x actual expected
                1 (codebreak showx)
                showy
          )
        );
        (* Complexity. *)
        tests |> iter (fun (ff, desc, xs) ->
          let candidate = candidate ff
          and reference = reference ff in
          xs |> iter (fun x ->
            let c0 = !c in
            let actual = candidate x in
            let actual_cost = !c - c0 in
            let c0 = !c in
            let expected = reference x in
            let expected_cost = !c - c0 in
            if actual_cost > expected_cost then
              let showx x = sprintf "%s, %d" desc x in
              mismatch_cost name x actual_cost expected_cost
                (codebreak showx)
          )
        );
        correct name
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* Memoized tree count. *)

let test_sigma () =
  section "Question 6" (
    let f i = i
    and show_f (_ : int -> int) = "(fun i -> i)" in
    let tests = [
      0, 10, f;
      1, 10, f;
      -5, 5, f;
      1, 1, f;
      1, 0, f;
    ] in
    test_value_3 "sigma" [%ty: int -> int -> (int -> int) -> int] sigma
      show_int show_int show_f show_int (=)
      tests
  )

let test_split_weight () =
  section "Question 7" (
    let f w1 w2 = 1 + w1 * w2
    and show_f (_ : int -> int -> int) = "(fun w1 w2 -> 1 + w1 * w2)" in
    let tests = map (fun w -> w, f) (up 0 10) in
    test_value_2 "split_weight" [%ty: int -> (int -> int -> int) -> int] split_weight
      show_int show_f show_int (=)
      tests
  )

let test_trees_of_weight () =
  section "Question 8" (
    test_value_1 "trees_of_weight" [%ty: int -> int] trees_of_weight
      show_int show_int (=) (up 0 20)
  )

let test_trees_of_weight_0_19 () =
  section "Question 9" (
    test_value_0 "trees_of_weight_0_19" [%ty: int list] trees_of_weight_0_19 (=)
  )

(* -------------------------------------------------------------------------- *)

(* Counting weight-balanced trees. *)

let test_split_wb_weight () =
  section "Question 10" (
    let f w1 w2 = 1 + w1 * w2
    and show_f (_ : int -> int -> int) = "(fun w1 w2 -> 1 + w1 * w2)" in
    let tests = map (fun w -> w, f) (up 0 10) in
    test_value_2 "split_wb_weight" [%ty: int -> (int -> int -> int) -> int]
      split_wb_weight
      show_int show_f show_int (=)
      tests
  )

let test_wb_trees_of_weight () =
  section "Question 11" (
    test_value_1 "wb_trees_of_weight" [%ty: int -> int] wb_trees_of_weight
      show_int show_int (=) (up 0 20)
  )

let test_wb_trees_of_weight_0_19 () =
  section "Question 12" (
    test_value_0 "wb_trees_of_weight_0_19" [%ty: int list]
      wb_trees_of_weight_0_19 (=)
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_weight() @
  test_height() @
  test_naive_trees_of_weight() @
  test_trees_of_weights() @
  test_fix() @
  test_sigma() @
  test_split_weight() @
  test_trees_of_weight() @
  test_trees_of_weight_0_19() @
  test_split_wb_weight() @
  test_wb_trees_of_weight() @
  test_wb_trees_of_weight_0_19() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
