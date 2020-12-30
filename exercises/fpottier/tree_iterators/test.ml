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

let spaced_block doc =
  nest 2 (break 1 ^^ doc) ^^ break 1

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

let apply f docs =
  raw_apply (utf8string f :: docs)

let wrap (print : 'a -> document) : 'a -> string =
  fun x -> pretty 70 (group (print x))

(* -------------------------------------------------------------------------- *)

(* An implementation of symbolic sequences. *)

module SymSeq = struct

  type _ seq =
  | Empty    : 'a seq
  | Singleton: 'a -> 'a seq
  | Sum      : int * 'a seq * 'a seq -> 'a seq
  | Product  : int * 'a seq * 'b seq -> ('a * 'b) seq
  | Map      : int * ('a -> 'b) * 'a seq -> 'b seq

  exception OutOfBounds

  let length (type a) (s : a seq) : int =
    match s with
    | Empty ->
        0
    | Singleton _ ->
        1
    | Sum (length, _, _) ->
        length
    | Product (length, _, _) ->
        length
    | Map (length, _, _) ->
        length

  let is_empty s =
    length s = 0

  let empty =
    Empty

  let singleton x =
    Singleton x

  let sum s1 s2 =
    if is_empty s1 then s2
    else if is_empty s2 then s1
    else Sum (length s1 + length s2, s1, s2)

  let bigsum ss =
    List.fold_left sum empty ss

  let product s1 s2 =
    if is_empty s1 || is_empty s2 then
      empty
    else
      Product (length s1 * length s2, s1, s2)

  let map phi s =
    if is_empty s then
      empty
    else
      Map (length s, phi, s)

  let rec get : type a . a seq -> int -> a =
    fun s i ->
      match s with
      | Empty ->
          raise OutOfBounds
      | Singleton x ->
          if i = 0 then x else raise OutOfBounds
      | Sum (_, s1, s2) ->
          let n1 = length s1 in
          if i < n1 then get s1 i
          else get s2 (i - n1)
      | Product (_, s1, s2) ->
          let q, r = i / length s2, i mod length s2 in
          get s1 q, get s2 r
      | Map (_, phi, s) ->
          phi (get s i)

  let rec foreach : type a . a seq -> (a -> unit) -> unit =
    fun s k ->
      match s with
      | Empty ->
          ()
      | Singleton x ->
          k x
      | Sum (_, s1, s2) ->
          foreach s1 k;
          foreach s2 k
      | Product (_, s1, s2) ->
          foreach s1 (fun x1 ->
            foreach s2 (fun x2 ->
              k (x1, x2)
            )
          )
      | Map (_, phi, s) ->
          foreach s (fun x -> k (phi x))

  let elements (s : 'a seq) : 'a list =
    let xs = ref [] in
    foreach s (fun x -> xs := x :: !xs);
    List.rev !xs

  (* A dependent product. Inefficient, as it enumerates the elements of [s1]
     and builds a large iterated sum. Use with caution. *)

  let bind (s1 : 'a seq) (s2 : 'a -> 'b seq) : 'b seq =
    let s = ref empty in
    foreach s1 (fun x1 ->
      s := sum !s (s2 x1)
    );
    !s

  (* Extract a list of at most [threshold] elements from the sequence [s]. *)

  let sample threshold (s : 'a seq) : 'a list =
    if length s <= threshold then
      (* If the sequence is short enough, keep of all its elements. *)
      elements s
    else
      (* Otherwise, keep a randomly chosen sample. *)
      let xs = ref [] in
      for i = 1 to threshold do
        let i = Random.int (length s) in
        let x = get s i in
        xs := x :: !xs
      done;
      !xs

end

type 'a seq =
  'a SymSeq.seq

(* -------------------------------------------------------------------------- *)

(* A fixed point combinator. *)

let fix : type a b . ((a -> b) -> (a -> b)) -> (a -> b) =
  fun ff ->
    let table = Hashtbl.create 128 in
    let rec f (x : a) : b =
      try
        Hashtbl.find table x
      with Not_found ->
        let y = ff f x in
        Hashtbl.add table x y;
        y
    in
    f

(* -------------------------------------------------------------------------- *)

(* MiniFeat. *)

module Feat = struct

  (* Core combinators. *)

  type 'a enum =
    int -> 'a SymSeq.seq

  let empty : 'a enum =
    fun _s ->
      SymSeq.empty

  let zero =
    empty

  let enum (xs : 'a SymSeq.seq) : 'a enum =
    fun s ->
      if s = 0 then xs else SymSeq.empty

  let just (x : 'a) : 'a enum =
    (* enum (SymSeq.singleton x) *)
    fun s ->
      if s = 0 then SymSeq.singleton x else SymSeq.empty

  let pay (enum : 'a enum) : 'a enum =
    fun s ->
      if s = 0 then SymSeq.empty else enum (s-1)

  let sum (enum1 : 'a enum) (enum2 : 'a enum) : 'a enum =
    fun s ->
      SymSeq.sum (enum1 s) (enum2 s)

  let ( ++ ) =
    sum

  let rec up_ i j =
    if i <= j then
      i :: up_ (i + 1) j
    else
      []

  let product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
    fun s ->
      SymSeq.bigsum (
        List.map (fun s1 ->
          let s2 = s - s1 in
          SymSeq.product (enum1 s1) (enum2 s2)
        ) (up_ 0 s)
      )

  let ( ** ) =
    product

  let balanced_product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
    fun s ->
      if s mod 2 = 0 then
        let s = s / 2 in
        SymSeq.product (enum1 s) (enum2 s)
      else
        let s = s / 2 in
        SymSeq.sum
          (SymSeq.product (enum1 s) (enum2 (s+1)))
          (SymSeq.product (enum1 (s+1)) (enum2 s))

  let ( *-* ) =
    balanced_product

  let map (phi : 'a -> 'b) (enum : 'a enum) : 'b enum =
    fun s ->
      SymSeq.map phi (enum s)

  (* Convenience functions. *)

  let finite (xs : 'a list) : 'a enum =
    List.fold_left (++) zero (List.map just xs)

  let bool : bool enum =
    just false ++ just true

  let list (elem : 'a enum) : 'a list enum =
    let cons (x, xs) = x :: xs in
    fix (fun list ->
      just [] ++ pay (map cons (elem ** list))
    )

  (* A dependent product. Inefficient, as it relies on [SymSeq.bind], which
     itself is inefficient. Use with caution. *)

  let bind (enum1 : 'a enum) (enum2 : 'a -> 'b enum) : 'b enum =
    fun s ->
      SymSeq.bigsum (
        List.map (fun s1 ->
          let s2 = s - s1 in
          SymSeq.bind (enum1 s1) (fun x1 -> enum2 x1 s2)
        ) (up_ 0 s)
      )

  (* Extract a list of at most [threshold] elements of each size,
     for every size up to [s] (excluded),
     from the enumeration [e]. *)

  let sample threshold s (e : 'a enum) : 'a list =
    List.flatten (
      List.map (fun i ->
        SymSeq.sample threshold (e i)
      ) (up_ 0 s)
    )

end

type 'a enum =
  'a Feat.enum

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

let todo =
  let text = [ R.Text "Not yet implemented." ] in
  [R.Message (text, R.Failure)]

let protect f =
  try
    T.run_timeout f
  with
  | Fail report ->
      report
  | TODO ->
      todo
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
  grab ty name (fun candidate ->
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

(* When doing black-box testing of a complete module, we are not testing just
   one function in isolation, but a group of functions together. In that case,
   the wording of the error message is somewhat different. Instead of saying
   that a specific function is incorrect, we want to say that an expression
   [expr] yields an incorrect result. *)

let eq_behavior eq_value actual_behavior expected_behavior =
  match actual_behavior, expected_behavior with
  | Ok actual, Ok expected ->
      eq_value actual expected (* value comparison *)
  | Error actual, Error expected ->
      actual = expected  (* exception comparison *)
  | Ok _, Error _
  | Error _, Ok _ ->
      false

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

let show_expected_behavior show_value behavior =
  match behavior with
  | Ok v ->
      R.Text "This is invalid. Producing the following result is valid:" ::
      R.Output (show_value v) ::
      []
  | Error e ->
      R.Text "This is invalid. Raising the following exception is valid:" ::
      R.Output (Printexc.to_string e) ::
      []

let something_is_wrong =
  R.Text "Something is wrong." ::
  []

let incorrect name =
  R.Code name :: R.Text "is incorrect." ::
  R.Break ::
  []

let black_box_compare
  (* Value equality and display, used to compare and show results. *)
  eq_value show_value
  (* The beginning of the error message. Use [something_is_wrong] or [incorrect name]. *)
  announcement
  (* Expression display. *)
  show_expr expr
  (* Actual behavior and expected behavior. *)
  actual_behavior
  expected_behavior
=
  (* Allow [TODO] to escape and abort the whole test. *)
  if actual_behavior = Error TODO then
    raise TODO
  else if not (eq_behavior eq_value actual_behavior expected_behavior) then
    fail (
      announcement @
      R.Text "The following expression:" ::
      R.Break ::
      R.Code (show_expr expr) ::
      R.Break ::
      show_actual_behavior show_value actual_behavior @
      show_expected_behavior show_value expected_behavior
    )

let test_value_1 name ty reference printx showy eqy tests =
  grab ty name (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun x ->
        let actual_behavior = T.result (fun () -> candidate x)
        and expected_behavior = T.result (fun () -> reference x) in
        let print_expr () =
          apply name [ printx x ]
            (* beware: [printx] must produce parentheses if necessary *)
        in
        black_box_compare
          eqy showy
          (incorrect name)
          (wrap print_expr) ()
          actual_behavior
          expected_behavior
        );
        correct name
    )
  )

let test_value_2 name ty reference printx1 printx2 showy eqy tests =
  grab ty name (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2) as x) ->
        let actual_behavior = T.result (fun () -> candidate x1 x2)
        and expected_behavior = T.result (fun () -> reference x1 x2) in
        let print_expr () =
          apply name [ printx1 x1; printx2 x2 ]
            (* beware: [printx1] and [printx2] must produce parentheses
               if necessary *)
        in
        black_box_compare
          eqy showy
          (incorrect name)
          (wrap print_expr) ()
          actual_behavior
          expected_behavior
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

(* -------------------------------------------------------------------------- *)

(* Some code is taken from the solution. *)

open Solution

(* -------------------------------------------------------------------------- *)

(* Printing trees. *)

let rec print_tree print_element (t : 'a tree) =
  group begin match t with
  | Leaf ->
      print_atomic_tree print_element t
  | Node (t0, x, t1) ->
      construct "Node" [
        print_tree print_element t0;
        print_element x;
        print_tree print_element t1
      ]
  end

and print_atomic_tree print_element (t : 'a tree) =
  group begin match t with
  | Leaf ->
      utf8string "Leaf"
  | Node (t0, x, t1) ->
      parens (print_tree print_element t)
  end

let print_atomic_tree_int =
  print_atomic_tree print_int

(* -------------------------------------------------------------------------- *)

(* Enumerating trees. *)

let enum_tree (elem : 'a enum) : 'a tree enum =
  let node (t1, (x, t2)) = Node (t1, x, t2) in
  Feat.(fix (fun tree ->
    just Leaf ++ pay (map node (tree ** elem ** tree))
  ))

let enum_int : int enum =
  Feat.finite [0; 1]

(* -------------------------------------------------------------------------- *)

(* Tree size. *)

let rec size t =
  match t with
  | Leaf ->
      0
  | Node (t0, _, t1) ->
      size t0 + 1 + size t1

(* -------------------------------------------------------------------------- *)

(* [slow_elements] *)

let trees : int tree list =
  let threshold = 100
  and size = 8 in
  Feat.sample threshold size (enum_tree enum_int)

let test_slow_elements () =
  section "Question 1" (
    test_value_1 "slow_elements" [%ty: int tree -> int list] slow_elements
      print_atomic_tree_int
      show_list_int (=)
      trees
  )

(* -------------------------------------------------------------------------- *)

(* [elements_with] *)

let tests : (int tree * int list) list =
  let threshold = 100
  and size = 8 in
  Feat.sample threshold size Feat.(enum_tree enum_int ** list enum_int)

let test_elements_with () =
  section "Question 2" (
    test_value_2 "elements_with" [%ty: int tree -> int list -> int list]
      elements_with
      print_atomic_tree_int
      print_list_int
      show_list_int (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* [elements] *)

let test_elements () =
  section "Question 3" (
    test_value_1 "elements" [%ty: int tree -> int list] elements
      print_atomic_tree_int
      show_list_int (=)
      trees
  )

(* -------------------------------------------------------------------------- *)

(* We could test a sequence by eagerly converting it to a list and testing
   the resulting list (i.e., comparing it with an expected list). However,
   a sequence can misbehave in various ways: it can diverge; it can yield
   irreproducible results if it has internal state. *)

(* Thus, we prefer to introduce a small DSL whose expressions inspect one or
   more sequences. We generate expressions in this DSL, and execute them,
   looking an incorrect behavior. (This may be a little overkill. We may end
   up exploring a very large space of expressions.) *)

(* The DSL evaluator is polymorphic in the type of the sequence elements,
   which must support OCaml's generic equality. *)

type index =
  int (* a de Bruijn index *)

type 'a expr =
  | EDone
  | ELetNilForce of index * 'a expr
      (* ELetNilForce (xs, e) means: *)
      (* let Nil = xs() in e *)
      (* binds no variable *)
  | ELetConsForce of 'a * index * 'a expr
      (* ELetConsForce (v, xs, e) means: *)
      (* let Cons (observed, ys) = xs() in assert(observed = v); e *)
      (* binds one variable *)

type 'a env =
  'a Seq.t list

(* An expression interpreter. *)

let rec eval (env : 'a env) (e : 'a expr) : unit =
  match e with
  | EDone ->
      ()
  | ELetNilForce (xs, e) ->
      let Seq.Nil = force (lookup env xs) in
      eval env e
  | ELetConsForce (v, xs , e) ->
      let Seq.Cons (observed, ys) = force (lookup env xs) in
      assert (observed = v);
      eval (ys :: env) e

and lookup env xs =
  assert (0 <= xs && xs < List.length env);
  List.nth env xs

and force xs =
  xs()

(* An expression enumerator. *)

(* This code is probably very inefficient. No memoization takes place. *)

(* The reference implementation is executed at the same time as enumeration
   takes place. This makes sense only because this implementation is pure.
   Thus, prefixes of executions can be shared. The environment [env] is the
   runtime environment of the reference implementation. *)

let rec expr (env : 'a env) : 'a expr enum =
  Feat.(
    just EDone ++
    pay (
      (* Choose a variable in scope. *)
      bind (finite (up 0 (List.length env))) (fun xs ->
        (* Query the sequence [xs] to see whether the oracle predicts
           [Nil] or [Cons]. *)
        match force (lookup env xs) with
        | Nil ->
            map (fun e -> ELetNilForce (xs, e)) (expr env)
        | Cons (v, vs) ->
            map (fun e -> ELetConsForce (v, xs, e)) (expr (vs :: env))
      )
    )
  )

(* An expression printer. The environment [senv] is the number of variables in
   scope. *)

type senv =
  int

let show_var (senv : senv) (x : index) : string =
  let x = senv - 1 - x in
  assert (0 <= x && x < 26);
  (Char.code 'a' + x) |> Char.chr |> String.make 1

let rec print_expr (show : 'a -> string) (senv : senv) (e : 'a expr) : document =
  match e with
  | EDone ->
      utf8string "()"
  | ELetNilForce (xs, e) ->
      utf8format "let Nil = %s() in" (show_var senv xs) ^^
      hardline ^^
      print_expr show senv e
  | ELetConsForce (v, xs, e) ->
      utf8format "let Cons (observed, %s) = %s() in"
        (show_var (senv+1) 0)
        (show_var senv xs) ^^
      hardline ^^
      utf8format "assert (observed = %s);" (show v) ^^
      hardline ^^
      print_expr show (senv+1) e

let print_scenario (show : 'a -> string) ((t, e) : 'a tree * 'a expr) : document =
  let print x = utf8string (show x) in
  utf8format "let %s =" (show_var 1 0) ^^ group (
    spaced_block (apply "fringe" [ print_tree print t ]) ^^
    utf8string "in"
  ) ^^
  hardline ^^
  print_expr show 1 e

let show_int_scenario =
  wrap (print_scenario show_int)

(* -------------------------------------------------------------------------- *)

(* [fringe] *)

let trees : int tree list =
  let threshold = 10
  and size = 4 in
  Feat.sample threshold size (enum_tree enum_int)

let test_fringe fringe =
  trees |> iter (fun t ->
    let threshold = 10
    and s = size t + 1
    and env = [Solution.fringe t] in
    let exprs = Feat.sample threshold s (expr env) in
    exprs |> iter (fun e ->
      let env = [fringe t] in
      try
        eval env e
      with exn ->
        let error =
          match exn with
          | Match_failure _  -> "a pattern matching failure"
          | Assert_failure _ -> "an assertion failure"
          | _                -> "an unknown failure" (* should not happen! *)
        in
        fail (
          R.Code "fringe" :: R.Text "is incorrect." ::
          R.Break ::
          R.Text ("The following test scenario causes " ^ error ^ ":") ::
          R.Output (show_int_scenario (t, e)) ::
          []
        )
    )
  );
  correct "fringe"

let test_fringe () =
  section "Question 4" (
    grab [%ty: int tree -> int Seq.t] "fringe" (fun fringe ->
      protect (fun () ->
        test_fringe fringe
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* A sequence printer. *)

(* Printing a sequence is problematic, as a sequence is a function, whose code
   could be arbitrary. The following printer is tailored to the sequences that
   we use as arguments when testing [equal] below. For this reason, it
   includes a special case for the sequence [trap]. *)

let rec print_atomic_seq print_element xs =
  if xs == Seq.trap then
    utf8string "Seq.trap"
  else
    group begin match xs() with
    | Seq.Nil ->
        utf8string "Seq.nil"
    | _ ->
        parens (print_seq print_element xs)
    end

and print_seq print_element xs =
  if xs == Seq.trap then
    utf8string "Seq.trap"
  else
    group begin match xs() with
    | Seq.Nil ->
        print_atomic_seq print_element xs
    | Seq.Cons (x, xs) ->
        apply "Seq.cons" [ print_element x; print_atomic_seq print_element xs ]
    end

let print_atomic_seq_int =
  print_atomic_seq print_int

(* -------------------------------------------------------------------------- *)

(* [equal] *)

(* To test this function, we enumerate ordinary (pure) sequences, obtained by
   enumerating lists and converting them to sequences. We also include a
   [trap] sequence, which raises an exception when forced. This allows us to
   verify that the function [equal] does not force the two sequences further
   than necessary. Note that the specification of [equal] does not indicate
   which of the two sequences should be forced first. This is not a problem
   here, as we use a single [Trap] exception. We would have several possible
   behaviors if we applied [equal] to two sequences that raise two distinct
   exceptions. *)

let seq (elem : 'a enum) : 'a Seq.t enum =
  Feat.(
    let cons (x, xs) = Seq.cons x xs in
    fix (fun seq ->
      just Seq.nil ++ just Seq.trap ++ pay (map cons (elem ** seq))
    )
  )

let seq_int : int Seq.t enum =
  Feat.(seq enum_int)

let tests : (int Seq.t * int Seq.t) list =
  Feat.(sample 10 5 (seq_int ** seq_int))

let test_equal () =
  section "Question 5" (
    test_value_2 "equal" [%ty: int Seq.t -> int Seq.t -> bool] equal
      print_atomic_seq_int
      print_atomic_seq_int
      show_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* [same_fringe] *)

let tests : (int tree * int tree) list =
  let threshold = 100
  and size = 8 in
  Feat.sample threshold size Feat.(enum_tree enum_int ** enum_tree enum_int)

let test_same_fringe () =
  section "Question 6" (
    test_value_2 "same_fringe" [%ty: int tree -> int tree -> bool]
      same_fringe
      print_atomic_tree_int
      print_atomic_tree_int
      show_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_slow_elements() @
  test_elements_with() @
  test_elements() @
  test_fringe() @
  test_equal() @
  test_same_fringe() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
