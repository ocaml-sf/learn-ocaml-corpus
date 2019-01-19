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

let comma = utf8string "," ^^ break 1

let int i = utf8string (string_of_int i)

let parens doc =
  utf8string "(" ^^ doc ^^ utf8string ")"

let parens doc =
  parens (nest 2 (break 0 ^^ doc) ^^ break 0)

(* -------------------------------------------------------------------------- *)

(* Basic functions on elements. *)

let print_element = function
  | Red -> "Red"
  | Yellow -> "Yellow"
  | Green -> "Green"

let print_element_option = function
  | None   -> "<none>"
  | Some x -> print_element x

let elem x = utf8string (print_element x)

let elems =
  [ Red; Yellow; Green ]

(* -------------------------------------------------------------------------- *)

(* List utilities. *)

(* The minimum element of a list of integers. *)

let minimum (ps : int list) : int =
  List.fold_left min max_int ps

(* Splitting a list at an element that satisfies the predicate [p]. *)

let rec split p xs accu =
  match xs with
  | [] ->
      (* No element that satisfies [p] was found. *)
      None
  | x :: xs ->
      if p x then
        (* Split at [x]. *)
        Some (List.rev accu, x, xs)
      else
        (* Continue searching. *)
        split p xs (x :: accu)

let split (p : 'a -> bool) (xs : 'a list) : ('a list * 'a * 'a list) option =
  split p xs []

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

(* [seqs n xs] is the list of all lists of length [n] whose elements are drawn
   from the list [xs]. *)

(* [seqsk n xs k] is [seqs n xs @ k]. *)

let rec seqsk n (xs : 'a list) (k : 'a list list) : 'a list list =
  if n = 0 then
    k
  else
    seqsk (n-1) xs (
      xs |> flat_map (fun x ->
        k |> List.map (fun xs -> x :: xs)
      )
    )

let seqs n (xs : 'a list) : 'a list list =
  seqsk n xs [[]]

(* -------------------------------------------------------------------------- *)

(* A multiset is implemented as a map of elements to positive integers. *)

module M = struct
  module M =
    Map.Make(struct
      type t = element
      let compare = compare
    end)
  type t = int M.t
  let empty =
    M.empty
  let multiplicity x m =
    try M.find x m with Not_found -> 0
  let bump x k m =
    M.add x (multiplicity x m + k) m
  let add x m =
    bump x 1 m
  let union m1 m2 =
    M.fold bump m1 m2
  let of_list xs =
    List.fold_right add xs empty
  let equal m1 m2 =
    (* Because an element is never mapped to zero, this works. *)
    M.equal (=) m1 m2
end

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

(* -------------------------------------------------------------------------- *)

(* The rank of a well-formed heap. *)

let rank =
  Solution.rank

(* The multiset of elements of a heap. *)

let rec elements (h : heap) : M.t =
  match h with
  | E ->
      M.empty
  | T (_, x, h1, h2) ->
      M.add x (M.union (elements h1) (elements h2))

(* [satisfies_heap_invariant p h] tests whether every element of [h] has
   priority at least [p] and [h] satisfies the heap invariant. *)

let rec satisfies_heap_invariant p h =
  match h with
  | E ->
      true
  | T (_, y, h1, h2) ->
      let q = priority y in
      p <= q && satisfies_heap_invariant q h1 && satisfies_heap_invariant q h2

(* Testing the heap invariant. *)

let satisfies_heap_invariant h =
  satisfies_heap_invariant min_int h

(* Testing the rank invariant. *)

let rec satisfies_rank_invariant h =
  match h with
  | E ->
      true
  | T (r, _, h1, h2) ->
      satisfies_rank_invariant h1 &&
      satisfies_rank_invariant h2 &&
      r = rank h2 + 1

(* Testing the leftist invariant. *)

let rec satisfies_leftist_invariant h : bool =
  match h with
  | E ->
      true
  | T (_, _, h1, h2) ->
      satisfies_leftist_invariant h1 &&
      satisfies_leftist_invariant h2 &&
      rank h1 >= rank h2

(* [build xs] prepares a tree of calls to the heap constructor [makeT], whose
   purpose is to construct a heap whose multiset of elements is given by the
   list [xs]. Such a tree is later evaluated by [eval]. Proceeding in two
   steps, with an explicit call tree in between, allows us to display the
   call tree if needed. *)

(* The function call [makeT x h1 h2] has the precondition that the priority
   of [x] must be less than or equal to the priority of any element of [h1]
   and [h2]. *)

module Tree = struct

  type tree =
    | E
    | MakeT of element * tree * tree
    | Union of tree * tree

  let rec build (xs : element list) : tree =
    match xs with
    | [] ->
        E
    | _ :: _ ->
        (* The list [xs] is nonempty. One of its minimum-priority elements
           must be the root of the heap. We split the list [xs] at such an
           element [x], yielding two sublists [lhs] and [rhs]. There remains
           to apply [build] to these sublists, yielding two subheaps, and to
           combine the pieces using [makeT]. *)
        let p = minimum (List.map priority xs) in
        match split (fun x -> priority x = p) xs with
        | None ->
            assert false (* impossible *)
        | Some (lhs, x, rhs) ->
            MakeT (x, build lhs, build rhs)

  let rec eval makeT ounion (t : tree) : heap =
    match t, ounion with
    | E, _ ->
        E
    | MakeT (x, t1, t2), _ ->
        makeT x (eval makeT ounion t1) (eval makeT ounion t2)
    | Union (t1, t2), Some union ->
        union (eval makeT ounion t1) (eval makeT ounion t2)

  (* A call tree printer. *)

  let rec print0 (t : tree) : document =
    group begin match t with
    | E ->
        utf8string "E"
    | MakeT _ ->
        parens (print1 t)
    end

  and print1 (t : tree) : document =
    group begin match t with
    | E ->
        print0 t
    | MakeT (x, t1, t2) ->
        utf8string "makeT" ^^ space ^^
        elem x ^^ space ^^
        print0 t1 ^^ space ^^
        print0 t2
    | Union (t1, t2) ->
        utf8string "union" ^^ space ^^
        print0 t1 ^^ space ^^
        print0 t2
    end

  let print t : string =
    pretty 40 (print1 t)

end

(* -------------------------------------------------------------------------- *)

(* A heap printer. *)

let rec print (h : heap) : document =
  group begin match h with
  | E ->
      utf8string "E"
  | T (r, x, h1, h2) ->
      utf8string "T" ^^ parens (
        int r ^^ comma ^^
        elem x ^^ comma ^^
        print h1 ^^ comma ^^
        print h2
      )
  end

let print h =
  pretty 40 (print h)

(* A printer for integers. *)

let show_int i =
  Printf.sprintf "%d" i

(* -------------------------------------------------------------------------- *)

(* Testing [rank]. *)

(* Here, we use a fixed number of test cases. *)

(* We check that the student reads the rank stored at the root
   (as opposed to computing the rank) by intentionally supplying
   ill-formed heaps whose rank information is incorrect. *)

let test_rank () =
  section "Question 1" (
    test_value_1 "rank" [%ty: heap -> rank] rank
      print show_int (=) [
        E;
        T(1, Red, E, E);
        T(2, Red, T(1, Red, E, E), T(1, Yellow, E, E));
        T(42, Red, E, E); (* invalid rank at root *)
      ]
  )

(* -------------------------------------------------------------------------- *)

(* Testing [makeT]. *)

(* We test [makeT] by checking that it produces a well-formed heap whose
   multiset of elements is correct. We do not test it by comparison by a
   reference implementation, because its specification is nondeterministic:
   there exist several correct implementations of [makeT]. For instance, in
   the case where [h1] and [h2] have the same rank, [makeT] may choose which
   of them becomes the left child and which becomes the right child. *)

(* We test each of the four postconditions separately, so the student may
   better see which postconditions are satisfied and which are not. This
   also produces a gradual grade, on a scale of 0 to 4. (It is debatable
   whether this is a good thing. Perhaps we should jump from grade 0 to
   grade 4.) *)

let n = 4
  (* No greater value is reasonable. *)

let seqs_ =
  Array.init (n+1) (fun i -> seqs i elems)

let partially_test_makeT (post, failure_msg, success_msg) makeT =
  (* Perform iterative deepening, so as to find a minimal example. *)
  for i = 1 to n do
    (* Generate all sequences of [i] elements. There are k^i such sequences,
       where [k] is the cardinal of the type [element]. *)
    seqs_.(i) |> List.iter (fun xs ->
      (* For each such sequence [xs], build a heap out of this sequence,
         using [makeT], and check that the postcondition is met. *)
      let t = Tree.build xs in
      let h = Tree.eval makeT None t in
      if not (post xs h) then
        fail [
          R.Text failure_msg;
          R.Break;
          R.Text "The following expression:";
          R.Break;
          R.Code (Tree.print t);
          R.Break;
          R.Text "produces the following invalid result:";
          R.Break;
          R.Output (print h);
        ]
      (* One might wish to pinpoint *where* and *why* the postcondition is
         violated. However, perhaps we have taken enough trouble already.
         The examples that we propose are small, and it might be useful for
         the student to take time and study them carefully. *)
    )
  done;
  (* Success. *)
  let points = 1 in
  [ R.success points success_msg ]

let tests = [

  (fun xs h -> satisfies_heap_invariant h),
  "The heap invariant is violated.",
  "The heap invariant seems respected.";

  (fun xs h -> satisfies_rank_invariant h),
  "The rank invariant is violated.",
  "The rank invariant seems respected.";

  (fun xs h -> satisfies_leftist_invariant h),
  "The leftist invariant is violated.",
  "The leftist invariant seems respected.";

  (fun xs h -> M.equal (M.of_list xs) (elements h)),
  "The multiset of elements is not preserved.",
  "The multiset of elements seems preserved.";

]

let test_makeT () =
  section "Question 2" (
    T.test_value
      (T.lookup_student [%ty : element -> heap -> heap -> heap] "makeT")
      (fun makeT ->
        tests |> flat_map (fun test ->
          protect (fun () ->
            partially_test_makeT test makeT
          )
        )
      )
  )

(* -------------------------------------------------------------------------- *)

(* Testing [singleton]. *)

(* Because [singleton] has a deterministic specification, it can be tested by
   comparison with a reference implementation. *)

let test_singleton () =
  section "Question 3" (
    test_value_1 "singleton" [%ty: element -> heap] Solution.singleton
      print_element print (=)
      elems
  )

(* -------------------------------------------------------------------------- *)

(* Testing [union]. *)

(* We test [union] by checking that it produces a heap whose multiset of
   elements is correct and where the heap invariant is respected. We do not
   test it by comparison by a reference implementation, because its
   specification is nondeterministic: there exist several correct
   implementations of [union]. *)

(* We do not test the rank invariant or the leftist invariant, because they
   cannot be violated if the student uses [makeT] as suggested. *)

(* We test the runtime complexity of [union] by counting how many calls to
   [priority] are made. (This is our only hook.) We assume that the student
   makes two calls to [priority] at each level. (It is possible to arrange for
   [priority] to be called just once per level, but a student is unlikely to
   do so.) *)

let partially_test_union (post, failure_msg, success_msg) makeT union =
  for i = 1 to n do
    seqs_.(i) |> List.iter (fun xs1 ->
      seqs_.(i) |> List.iter (fun xs2 ->
        let t1 = Tree.build xs1
        and t2 = Tree.build xs2 in
        let t = Tree.Union (t1, t2) in
        let h = Tree.eval makeT (Some union) t in
        if not (post (xs1 @ xs2) h) then
          fail [
            R.Text failure_msg;
            R.Break;
            R.Text "The following expression:";
            R.Break;
            R.Code (Tree.print t);
            R.Break;
            R.Text "produces the following invalid result:";
            R.Break;
            R.Output (print h);
          ];
      )
    )
  done;
  (* Success. *)
  let points = 1 in
  [ R.success points success_msg ]

let tests = [

  (fun xs h -> satisfies_heap_invariant h),
  "The heap invariant is violated.",
  "The heap invariant seems respected.";

  (fun xs h -> M.equal (M.of_list xs) (elements h)),
  "The multiset of elements is not preserved.",
  "The multiset of elements seems preserved.";

]

(* To find potential faults in the complexity of [union], we use imbalanced
   (left-leaning) heaps. Remarkably, for every [n], it is possible to build a
   valid heap of rank 2 whose left spine has length [n]. We do not care about
   the elements of these heaps: we take them all equal, so the heap invariant
   is automatically respected. *)

let rec imbalanced n =
  let x = Red in
  let singleton = Tree.(MakeT (x, E, E)) in
  if n = 0 then
    singleton
  else
    Tree.MakeT (x, imbalanced (n - 1), singleton)

let m = 10

let test_union_complexity makeT union =
  for i = 0 to m do
    let t1 = imbalanced i in
    let h1 = Tree.eval makeT None t1 in
    for j = 0 to m do
      let t2 = imbalanced j in
      let h2 = Tree.eval makeT None t2 in
      let r1 = rank h1
      and r2 = rank h2 in
      let t = Tree.Union (t1, t2) in
      calls_to_priority := 0;
      let h = union h1 h2 in
      let cost = !calls_to_priority / 2 in
      let predicted_cost = r1 + r2 in
      if predicted_cost < cost then
        fail [
          R.Text "The complexity of union seems incorrect.";
          R.Break;
          R.Text "The following expression:";
          R.Break;
          R.Code (Tree.print t);
          R.Break;
          R.Text (Printf.sprintf "causes %d priority comparisons, \
                  while we estimate that %d comparisons should suffice."
                    cost predicted_cost);
        ]
    done
  done;
  (* Success. *)
  let points = 1 in
  [ R.success points "The complexity of union seems correct." ]

let test_union makeT union =
  let correctness =
    flat_map (fun test ->
      protect (fun () ->
        partially_test_union test makeT union
      )
    ) tests
  in
  (* Complexity is tested only if [union] seems functionally correct.
     We do not want the student to worry about complexity too early. *)
  if successful correctness then
    correctness @
    protect (fun () ->
      test_union_complexity makeT union
    )
  else
    correctness

let test_union () =
  section "Question 4" (
    T.test_value
      (T.lookup_student [%ty : element -> heap -> heap -> heap] "makeT")
      (fun makeT ->
        T.test_value
          (T.lookup_student [%ty : heap -> heap -> heap] "union")
          (fun union ->
             test_union makeT union
          )
      )
  )

(* -------------------------------------------------------------------------- *)

(* Testing [insert] and [extract]. *)

(* Although we could use a form of white-box testing, as we did for [union],
   and test whether these functions satisfy their postconditions, we adopt a
   different strategy. We perform black-box testing: we execute tiny programs
   expressed in a DSL whose operations are [empty], [insert], and [extract],
   and we check that the observable outputs of these programs are correct. *)

(* We do not test [singleton] and [union], as we have tested them already. *)

(* We do not test that heaps are actually persistent, as the student is very
   unlikely to introduce mutable state and break persistence. Thus, our DSL
   does not need to have multiple variables of type [heap]. One heap suffices.
   Therefore, a program in this DSL is a sequence of [insert] and [extract]
   instructions. *)

type instruction =
  | IInsert of element
  | IExtract

type instructions =
  instruction list

module DSLInterpreter (H : sig
  val insert: element -> heap -> heap
  val extract: heap -> (element * heap) option
end) = struct
  open H

  (* The interpreter inhabits the state/output monad, where the state is the
     current heap, and the output is the list of extracted elements. *)

  type 'a m =
    heap -> 'a * heap * element option list

  let return (x : 'a) : 'a m =
    fun h ->
      x, h, []

  let (>>=) (m : 'a m) (k : 'a -> 'b m) : 'b m =
    fun h ->
      let x, h, output1 = m h in
      let y, h, output2 = k x h in
      y, h, output1 @ output2

  let interpret1 (i : instruction) : unit m = fun h ->
    match i with
    | IInsert x ->
        (), insert x h, []
    | IExtract ->
        match extract h with
        | None ->
            (), h, [None]
        | Some (x, h) ->
            (), h, [Some x]

  let rec interpret (is : instructions) : unit m =
    match is with
    | [] ->
        return ()
    | i :: is ->
        interpret1 i >>= fun () ->
        interpret is

  let print_output (oxs : element option list) : string =
    String.concat "; " (List.map print_element_option oxs)

  let run (is : instructions) : string =
    let empty : heap = E in
    let (), (_ : heap), output = interpret is empty in
    print_output output

end

module DSLPrinter = struct

  let print1 (i : instruction) : string =
    match i with
    | IInsert x ->
        Printf.sprintf "insert %s" (print_element x)
    | IExtract ->
        Printf.sprintf "extract"

  let print (is : instructions) : string =
    List.fold_left (fun accu i ->
      Printf.sprintf "%s; %s" accu (print1 i)
    ) "empty" is

end

let test_insert_extract insert extract (is : instructions) =
  let module Reference = DSLInterpreter(struct
    let insert = Solution.insert
    let extract = Solution.extract
  end) in
  let module Candidate = DSLInterpreter(struct
    let insert = insert
    let extract = extract
  end) in
  (* Run the two implementations, and compare their observable outputs. *)
  let expected = Reference.run is
  and actual = Candidate.run is in
  if expected <> actual then
    fail [
      R.Text "One of insert or extract is incorrect.";
      R.Break;
      R.Text "The following sequence of operations:";
      R.Break;
      R.Code (DSLPrinter.print is);
      R.Break;
      R.Text "gives rise to the following sequence of extracted elements:";
      R.Break;
      R.Output actual;
      R.Text "whereas the expected sequence is as follows:";
      R.Break;
      R.Output expected;
    ]

let instruction_set : instructions =
  IExtract ::
  List.map (fun x -> IInsert x) elems

let test_insert_extract insert extract =
  protect (fun () ->
    (* Use iterative deepening, so as to find an instruction sequence
       of minimal length that exhibits a violation of the spec. *)
    for n = 1 to 6 do
      seqs n instruction_set |>
        List.iter (test_insert_extract insert extract)
    done;
    (* Success. *)
    let points = 1 in
    [ R.success points "insert and extract seem correct." ]
  )

let test_insert_extract () =
  section "Questions 5 and 6" (
    T.test_value
      (T.lookup_student [%ty : element -> heap -> heap] "insert")
      (fun insert ->
        T.test_value
          (T.lookup_student [%ty : heap -> (element * heap) option] "extract")
          (fun extract ->
             test_insert_extract insert extract
          )
      )
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_rank() @
  test_makeT() @
  test_singleton() @
  test_union() @
  test_insert_extract() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
