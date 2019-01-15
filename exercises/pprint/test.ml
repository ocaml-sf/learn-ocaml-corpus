open Printf
let iter = List.iter
let map = List.map
module T = Test_lib
module R = Report
type report = R.t

(* The auto-grader. *)

(* -------------------------------------------------------------------------- *)

(* Generic testing utilities. *)

(* When we fail, the exception carries a learn-ocaml report. *)

exception Fail of report

(* [section title report] encloses the report [report] within a section
   entitled [title], producing a larger report. *)

let section title report : report =
  [R.Section ([R.Text title], report)]

(* [within_section title action] evaluates [action()], which either returns
   normally and produces a report, or fails and produces a report. In either
   case, the report is enclosed within a section entitled [title]. *)

let within_section title action : report =
  try
    let report = action() in
    section title report
  with Fail report ->
    raise (Fail (section title report))

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

(* -------------------------------------------------------------------------- *)

(* Generic test functions. *)

let test1 name candidate reference showx showy eqy xs =
  xs |> List.iter (fun x ->
    let actual = candidate x
    and expected = reference x in
    if not (eqy actual expected) then
      fail [
        R.Code name; R.Text "is incorrect.";
        R.Break;
        R.Text "When applied to the following argument:";
        R.Break;
        R.Code (showx x);
        R.Break;
        R.Text "it produces the following invalid result:";
        R.Break;
        R.Output (showy actual);
        R.Text "A valid result is:";
        R.Break;
        R.Output (showy expected);
      ]
  );
  let message = [ R.Code name; R.Text "seems correct."; ] in
  [ R.Message (message, R.Success 1) ]

let test_value_1 name ty reference showx showy eqy xxs =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      test1 name candidate reference showx showy eqy xxs
    )
  )

let test2 name candidate reference showx1 showx2 showy eqy xxs =
  xxs |> List.iter (fun (x1, x2) ->
    let actual = candidate x1 x2
    and expected = reference x1 x2 in
    if not (eqy actual expected) then
      fail [
        R.Code name; R.Text "is incorrect.";
        R.Break;
        R.Text "When applied to the following arguments:";
        R.Break;
        R.Code (showx1 x1);
        R.Break;
        R.Code (showx2 x2);
        R.Break;
        R.Text "it produces the following invalid result:";
        R.Break;
        R.Output (showy actual);
        R.Text "A valid result is:";
        R.Break;
        R.Output (showy expected);
      ]
  );
  let message = [ R.Code name; R.Text "seems correct."; ] in
  [ R.Message (message, R.Success 1) ]

let test_value_2 name ty reference showx1 showx2 showy eqy xxs =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      test2 name candidate reference showx1 showx2 showy eqy xxs
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

(* -------------------------------------------------------------------------- *)

(* Continuation-based enumerations. *)

(* [split n k] enumerates all manners of splitting [n] into [n1 + n2], where
   [n1] and [n2] can be zero. *)

let split n k =
  for n1 = 0 to n do
    let n2 = n - n1 in
    k n1 n2
  done

(* [list f] converts the continuation-based producer [f] to a list. *)

let list f =
  let xs = ref [] in
  f (fun x -> xs := x :: !xs);
  List.rev !xs

(* If [f i] is an enumeration, then [deepening f n] is the concatenation
   of the enumerations [f 0, f 1, ... f n]. *)

let deepening f n k =
  for i = 0 to n do
    f i k
  done

(* -------------------------------------------------------------------------- *)

(* Basic functions on requirements and documents are taken from the solution. *)

open Solution

(* -------------------------------------------------------------------------- *)

(* Well-formedness of documents. *)

let rec wf (doc : doc) : bool =
  match doc with
  | Empty
  | HardLine ->
      true
  | Char c ->
      c <> '\n'
  | Cat (req, doc1, doc2) ->
      wf doc1 && wf doc2 &&
      req = requirement doc1 ++ requirement doc2
  | Nest (_, req, doc)
  | Group (req, doc) ->
      wf doc && req = requirement doc
  | IfFlat (IfFlat _, _) ->
      false
  | IfFlat (doc1, doc2) ->
      wf doc1 && wf doc2

(* -------------------------------------------------------------------------- *)

(* A document normalization function performs certain simplifications. *)

(* This yields a document comparison function that ignores certain superficial
   differences between documents. We use this function when comparing the
   student's solution against ours, so the student's code is considered
   correct, regardless of whether it implements our simplifications. *)

(* Here, we may assume that the document is well-formed. Whether the student's
   document is well-formed is checked separately. *)

let rec normalize doc =
  match doc with
  | Empty ->
      empty
  | HardLine ->
      hardline
  | Char c ->
      char c
  | Cat (_, doc1, doc2) ->
      normalize doc1 ^^ normalize doc2
  | Nest (i, _, doc) ->
      nest i (normalize doc)
  | Group (_, doc) ->
      group (normalize doc)
  | IfFlat (doc1, doc2) ->
      ifflat (normalize doc1) (normalize doc2)

let eq_doc doc1 doc2 =
  normalize doc1 = normalize doc2

let wf_eq_doc actual expected =
  wf actual && eq_doc actual expected

(* -------------------------------------------------------------------------- *)

(* An enumerator of well-formed documents of size [n]. *)

(* We generate these documents by using our smart constructors, so they are
   really normalized versions of underlying unnormalized documents of [n]
   nodes. This implies that they can have fewer than [n] nodes. This also
   implies that the list can have repeated elements. *)

let rec docs n k =
  if n = 0 then begin
    k Empty;
    k HardLine;
    k (Char 'a')
  end
  else begin
    let n = n - 1 in
    doc_pairs n (fun (doc1, doc2) ->
      k (doc1 ^^ doc2)
    );
    docs n (fun doc ->
      k (nest 2 doc)
    );
    docs n (fun doc ->
      k (group doc)
    );
    doc_pairs n (fun (doc1, doc2) ->
      k (ifflat doc1 doc2)
    );
  end

and doc_pairs n k =
  split n (fun n1 n2 ->
    docs n1 (fun doc1 ->
      docs n2 (fun doc2 ->
        k (doc1, doc2)
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* Printers. *)

(* A printer for strings! *)

let show_string s =
  (* No escaping, no quoting. This is used to show the output of [pretty]. *)
  s
  (* sprintf "\"%s\"" (String.escaped s) *)

(* A printer for integers. *)

let show_int i =
  sprintf "%d" i

(* A printer for characters. *)

let show_char c =
  sprintf "'%s'" (Char.escaped c)

(* A printer for Booleans. *)

let show_bool b =
  if b then "true" else "false"

(* A printer for requirements in OCaml syntax. *)

let show_req req =
  match req with
  | Infinite ->
      "Infinite"
  | Finite i ->
      sprintf "Finite %d" i

let print_req req =
  string (show_req req)

(* A printer for documents in OCaml syntax. *)

(* I am using PPrint itself (as implemented in the solution!) to pretty-print
   PPrint documents. *)

let break0 =
  ifflat empty hardline

let break1 =
  ifflat (char ' ') hardline

let pre_comma doc =
  char ',' ^^ break1 ^^ doc

let pre_commas docs =
  concat (map pre_comma docs)

let commas docs =
  match docs with
  | [] ->
      empty
  | doc :: docs ->
      doc ^^ pre_commas docs

let tuple docs =
  group (
    char '(' ^^
    nest 2 (break0 ^^ commas docs) ^^ break0 ^^
    char ')'
  )

let construct label docs =
  string label ^^ char ' ' ^^ tuple docs

let rec print doc : doc =
  match doc with
  | Empty ->
      string "Empty"
  | HardLine ->
      string "HardLine"
  | Char c ->
      string (sprintf "Char %s" (show_char c))
  | Cat (req, doc1, doc2) ->
      construct "Cat" [print_req req; print doc1; print doc2]
  | Nest (i, req, doc) ->
      construct "Nest" [string (sprintf "%d" i); print_req req; print doc]
  | Group (req, doc) ->
      construct "Group" [print_req req; print doc]
  | IfFlat (doc1, doc2) ->
      construct "IfFlat" [print doc1; print doc2]

let show_doc doc =
  pretty 70 (print doc)

(* -------------------------------------------------------------------------- *)

(* A printer and generator for a type of binary trees. *)

(* The documents thus obtained are used as test cases to test the student's
   rendering engine. *)

(* Here, we use our own smart constructors, not the student's. *)

type tree =
  | L
  | N of tree * int * tree

let rec print_tree t =
  match t with
  | L ->
      char 'L'
  | N (t1, x, t2) ->
      construct "N" [print_tree t1; string (show_int x); print_tree t2]

let rec trees n k =
  if n = 0 then
    k L
  else
    let n = n - 1 in
    split n (fun n1 n2 ->
      trees n1 (fun t1 ->
        trees n2 (fun t2 ->
          let x = Random.int 10 in
          k (N (t1, x, t2))
        )
      )
    )

(* -------------------------------------------------------------------------- *)

(* Operations on requirements. *)

let tests =
  [
    Finite 0, Finite 0;
    Finite 3, Finite 4;
    Finite 4, Finite 3;
    Finite 4, Infinite;
    Infinite, Finite 4;
    Infinite, Infinite;
  ]

let test_addition () =
  within_section "Question 1" (fun () ->
    test_value_2 "++" [%ty : req -> req -> req] (++)
      show_req show_req show_req (=)
      tests
  )

let test_comparison () =
  within_section "Question 2" (fun () ->
    test_value_2 "<==" [%ty : req -> req -> bool] (<==)
      show_req show_req show_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Requirement. *)

let docs_1 =
  list (deepening docs 1)

(* The following ill-formed documents are used to test [requirement].
   These documents have an invalid requirement at the root. We check
   that [requirement] returns this requirement. This suggests that
   [requirement] has the correct complexity: it does not recompute
   a correct requirement by traversing the whole tree. *)

let poison =
  [
    Cat (Infinite, Empty, Empty);
    Nest (0, Infinite, Empty);
    Group (Infinite, Empty);
  ]

let test_requirement () =
  within_section "Question 3" (fun () ->
    test_value_1 "requirement" [%ty : doc -> req] requirement
      show_doc show_req (=)
      (docs_1 @ poison)
  )

(* -------------------------------------------------------------------------- *)

(* Smart constructors. *)

let doc_pairs_2=
  list (deepening doc_pairs 2)

let test_smart_constructors () =
  within_section "Question 4" (fun () ->
    (* No need to test [empty] and [hardline]. *)
    test_value_1 "char" [%ty : char -> doc] char
      show_char show_doc wf_eq_doc
      [ 'a'; '\n' ]
    @
    test_value_2 "^^" [%ty: doc -> doc -> doc] (^^)
      show_doc show_doc show_doc wf_eq_doc
      doc_pairs_2
    @
    test_value_2 "nest" [%ty: int -> doc -> doc] nest
      show_int show_doc show_doc wf_eq_doc
      (map (fun doc -> 2, doc) docs_1)
    @
    test_value_1 "group" [%ty : doc -> doc] group
      show_doc show_doc wf_eq_doc
      docs_1
    @
    test_value_2 "ifflat" [%ty: doc -> doc -> doc] ifflat
      show_doc show_doc show_doc wf_eq_doc
      doc_pairs_2
    @
    []
  )

(* -------------------------------------------------------------------------- *)

(* The rendering engine. *)

(* We must test with very narrow widths,
   otherwise the documents that exhibit a
   problem are huge. *)

let tests =
  list (deepening trees 5)
  |> map print_tree
  |> flat_map (fun doc -> [8, doc; 16, doc])

let test_pretty () =
  within_section "Question 5" (fun () ->
    test_value_2 "pretty" [%ty : int -> doc -> string] pretty
      show_int show_doc show_string (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_addition() @
  test_comparison() @
  test_requirement() @
  test_smart_constructors() @
  test_pretty() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
