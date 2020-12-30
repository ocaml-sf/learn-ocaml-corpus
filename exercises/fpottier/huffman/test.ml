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

(* Miscellaneous. *)

let postincrement c =
  let n = !c in
  c := n + 1;
  n

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

let block b doc =
  nest 2 (break b ^^ doc) ^^ break b

let parens doc =
  utf8string "(" ^^ block 0 doc ^^ utf8string ")"

let brackets doc =
  utf8string "[" ^^ block 0 doc ^^ utf8string "]"

let ocaml_array_brackets doc =
  utf8string "[| " ^^ block 0 doc ^^ utf8string "|]"

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

let parens_apply f docs =
  parens (apply f docs)

let piped_apply f docs =
  (* Isolate the last argument. *)
  assert (List.length docs > 0);
  let docs = List.rev docs in
  let doc, docs = List.hd docs, List.rev (List.tl docs) in
  (* Print. *)
  group (doc ^^ break 1 ^^ utf8string "|>" ^^ space ^^ apply f docs)

let infix_apply op doc1 doc2 =
  group (
    group (doc1 ^^ break 1 ^^ utf8string op) ^^ break 1 ^^
    doc2
  )

let elet x doc1 doc2 =
  group (
    group (
      utf8format "let %s =" x ^^
      block 1 doc1 ^^
      utf8string "in"
    ) ^^ break 1 ^^
    doc2
  )

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

  let check length =
    assert (length >= 0); (* if this fails, an overflow has occurred *)
    length

  let sum s1 s2 =
    if is_empty s1 then s2
    else if is_empty s2 then s1
    else Sum (check (length s1 + length s2), s1, s2)

  let bigsum ss =
    List.fold_left sum empty ss

  let product s1 s2 =
    if is_empty s1 || is_empty s2 then
      empty
    else
      Product (check (length s1 * length s2), s1, s2)

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

  (* For some reason, [Random.int] stops working at [2^30]. *)

  let rec random_int n =
    let threshold = 1 lsl 30 in
    if n < threshold then
      Random.int n
    else
      failwith "Can't sample over more than 2^30 elements."

  (* Extract a list of at most [threshold] elements from the sequence [s]. *)

  let sample threshold (s : 'a seq) : 'a list =
    if length s <= threshold then
      (* If the sequence is short enough, keep of all its elements. *)
      elements s
    else
      (* Otherwise, keep a randomly chosen sample. *)
      let xs = ref [] in
      for i = 1 to threshold do
        let i = random_int (length s) in
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

let   curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let fix2 : type a b c . ((a -> b -> c) -> (a -> b -> c)) -> (a -> b -> c) =
  fun ff ->
    let ff f = uncurry (ff (curry f)) in
    curry (fix ff)

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

  let rec _up i j =
    if i <= j then
      i :: _up (i + 1) j
    else
      []

  let product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
    fun s ->
      SymSeq.bigsum (
        List.map (fun s1 ->
          let s2 = s - s1 in
          SymSeq.product (enum1 s1) (enum2 s2)
        ) (_up 0 s)
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

  let nonempty_list (elem : 'a enum) : 'a list enum =
    let cons (x, xs) = x :: xs in
    map cons (elem ** list elem)

  (* Extract a list of at most [threshold] elements of each size,
     for every size up to [s] (included), from the enumeration [e]. *)

  let sample threshold s (e : 'a enum) : 'a list =
    List.flatten (
      List.map (fun i ->
        SymSeq.sample threshold (e i)
      ) (_up 0 s)
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

let correct2 name1 name2 =
  let message = [ R.Code name1; R.Text "and"; R.Code name2; R.Text "seem correct."; ] in
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

let test_value_1_in_context
  (* name and type of the function of interest: *)
  name ty
  (* reference implementation of this function: *)
  reference
  (* input transformation function and input printer: *)
  make_input print_input
  (* output transformation function and output printer: *)
  context print_context
  (* observation equality test and observation printer: *)
  eq show_observation
  (* list of inputs: *)
  tests
=
  (* Beware: [print_input] must produce parentheses if necessary. Also,
     [print_context] must enclose its argument in parentheses if necessary. *)
  grab ty name (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun x ->
        let actual_behavior =
          T.result (fun () ->
            context (candidate (make_input x))
          )
        and expected_behavior =
          T.result (fun () ->
              context (reference (make_input x))
          )
        in
        let print () =
          print_context (apply name [ print_input x ])
        in
        black_box_compare
          eq show_observation
          (incorrect name)
          (wrap print) ()
          actual_behavior
          expected_behavior
      );
      correct name
    )
  )

let identity x = x

let test_value_1 name ty reference printx eqy showy tests =
  test_value_1_in_context
    name ty reference
    identity printx
    identity identity (* no context *)
    eqy showy tests

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

let test_value_3 name ty reference printx1 printx2 printx3 showy eqy tests =
  grab ty name (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2, x3) as x) ->
        let actual_behavior = T.result (fun () -> candidate x1 x2 x3)
        and expected_behavior = T.result (fun () -> reference x1 x2 x3) in
        let print_expr () =
          apply name [ printx1 x1; printx2 x2; printx3 x3 ]
            (* beware: [printx1], etc. must produce parentheses
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

let print_string s =
  utf8string (show_string s)

(* A printer for integers. *)

let print_int =
  int

let show_int i =
  sprintf "%d" i

(* A printer for characters. *)

let show_char c =
  sprintf "'%s'" (Char.escaped c)

let print_char c =
  utf8string (show_char c)

(* A printer for Booleans. *)

let show_bool b =
  if b then "true" else "false"

let print_bool b =
  utf8string (show_bool b)

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

(* Enumerating all paths into a tree. *)

let rec paths prefix (t : tree) accu =
  match t with
  | Leaf _ ->
      prefix :: accu
  | Node (t0, t1) ->
      paths (prefix ^ "0") t0 (
        paths (prefix ^ "1") t1
          accu
      )

let paths t =
  paths "" t []

(* -------------------------------------------------------------------------- *)

(* A generator for trees. *)

(* We want to generate trees with distinct leaves. The easiest way of doing so
   seems to be to first generate trees with arbitrary data at the leaves, then
   (in a separate pass) populate the leaves with distinct characters. *)

module Generate = struct

  open Feat

  let node (t1, t2) =
    Node (t1, t2)

  (* A tree of size [n] has [n] nodes, therefore [n+1] leaves. *)

  let rec tree : tree enum =
    fix (fun tree ->
      just (Leaf 'z') ++
      pay (map node (tree ** tree))
    )

  let decorate tree =
    let c = ref 0 in
    let next () : char =
      Char.chr (postincrement c + Char.code 'a')
    in
    let rec decorate tree =
      match tree with
      | Leaf _ ->
          Leaf (next())
      | Node (t0, t1) ->
          Node (decorate t0, decorate t1)
    in
    decorate tree

  let all_trees_with_distinct_leaves n =
    (* Generate all trees of size [n], then decorate them. Note that we
       do not need to enumerate all permutations of characters at the
       leaves; one permutation suffices to exercise the student's code. *)
    SymSeq.elements (tree n) |> List.map decorate

  (* A generator for strings. *)

  let character : string enum =
    finite [ "a"; "b"; "c"; "d" ]

  let glue : string list -> string =
    String.concat ""

  let string : string enum =
    list character |> map glue

  (* A generator for strings where each character has a distinct frequency. *)

  let successor (c : char) : char =
    Char.chr (Char.code c + 1)

  let rec distinct_frequency_string (start : char) n =
    if n = 0 then
      ""
    else
      String.make n start ^
      distinct_frequency_string (successor start) (n - 1)

end

(* -------------------------------------------------------------------------- *)

(* A printer for trees. *)

let rec print_tree tree =
  match tree with
  | Leaf c ->
      utf8string "Leaf" ^^ space ^^ print_char c
  | Node (t0, t1) ->
      construct "Node" [ print_tree t0; print_tree t1 ]

(* Miscellaneous printers. *)

let show_tree =
  wrap print_tree

let print_pair print_x print_y (x, y) =
  tuple [ print_x x; print_y y ]

let print_char_list =
  print_list print_char

let show_char_list =
  wrap print_char_list

let print_char_string_list =
  print_list (print_pair print_char print_string)

let show_char_string_list =
  wrap print_char_string_list

let print_char_freq_list =
  print_list (print_pair print_char print_int)

let show_char_freq_list =
  wrap print_char_freq_list

let print_char_int =
  print_pair print_char print_int

let show_char_int =
  wrap print_char_int

(* -------------------------------------------------------------------------- *)

(* Determining whether a list is sorted. *)

(* If it is not sorted, we produce an inversion (a pair that is not sorted). *)

let rec is_sorted cmp xs =
  match xs with
  | []
  | [_] ->
      None
  | x0 :: ((x1 :: _) as xs) ->
      if cmp x0 x1 <= 0 then
        is_sorted cmp xs
      else
        Some (x0, x1)

(* -------------------------------------------------------------------------- *)

(* Determining whether a string contains only '0' and '1' characters. *)

let string_for_all f s =
  let exception Break in
  try
    for i = 0 to String.length s - 1 do
      if not (f s.[i]) then raise Break
    done;
    true
  with Break ->
    false

let is_binary_char c =
  match c with '0' | '1' -> true | _ -> false

let is_binary_string s =
  string_for_all is_binary_char s

(* -------------------------------------------------------------------------- *)

(* Converting a single character to an 8-character string of '0' and '1'. *)

(* [write_char] and [read_char] are in [prelude.ml]. The functions below
   can be used for testing in the OCaml REPL. *)

let encode_char (c : char) : string =
  let b = Buffer.create 8 in
  write_char b c;
  Buffer.contents b

(* And back. *)

let next (s : string) : unit -> char =
  let i = ref 0 in
  fun () ->
    s.[postincrement i]

let decode_char (data : data) : char =
  read_char (next data)

(* -------------------------------------------------------------------------- *)

(* Grading [build_alphabet]. *)

(* We test [build_alphabet] in the context [entries] (a function
   defined in Prelude), which yields a sorted list of character-
   frequency pairs. *)

let long_sentence =
  "It is amazing that such a seemingly short sentence can be \
   successfully compressed."

let inputs =
  (* Short strings *)
  Feat.(sample 20 4 Generate.string) @
  (* Short strings where all characters have distinct frequency *)
  List.map (Generate.distinct_frequency_string 'a') (up 2 10) @
  (* A few long strings *)
  "I am obviously right." ::
  "The quick brown fox jumps over the lazy dog." ::
  "alpha bravo alpha bravo bravo alpha bravo alpha alpha" ::
  "Ça doit marcher avec des accents français aussi, eh oui, c'est énervant" ::
  []

let print_entries x =
  apply "entries" [ parens x ]

let test_build_alphabet () =
  section "Question 1" (
    test_value_1_in_context
      "build_alphabet" [%ty: text -> alphabet] Solution.build_alphabet
      identity print_string
      (* context: *) entries print_entries
      (=) show_char_freq_list
      inputs
  )

(* In the following, we must be careful to apply [build_tree] only to
   alphabets of two characters at least. *)

let inputs =
  List.filter (fun input ->
    List.length (entries (Solution.build_alphabet input)) >= 2
  ) inputs

(* -------------------------------------------------------------------------- *)

(* Grading [build_tree]. *)

(* Because printing an alphabet is impossible (it is a hash table), we prefer
   to use strings as inputs, rather than alphabets. This means that we must
   test the composition of [build_alphabet] and [build_tree], rather than
   [build_tree] alone. *)

(* We cannot expect the tree built by the student to have exactly the same
   shape as ours; the construction algorithm is nondeterministic, due to
   possible draws between priorities. We first check that the leaves of the
   tree form the alphabet. Then, we check that the tree is optimal, in the
   sense that it yields an encoded input of minimal length. *)

let test_build_tree () =
  let name = "build_tree" in
  section "Question 2" (
    grab [%ty: text -> alphabet] "build_alphabet" (fun build_alphabet ->
    grab [%ty: alphabet -> tree] "build_tree" (fun build_tree ->
    protect (fun () ->
      (* Check 1. *)
      inputs |> List.iter begin fun input ->
        let actual_behavior =
          T.result (fun () ->
            sort (leaves (build_tree (build_alphabet input)))
          )
        and expected_behavior =
          T.result (fun () ->
            Solution.(sort (leaves (build_tree (build_alphabet input))))
          )
        in
        let print () =
          piped_apply "sort" [
            piped_apply "leaves" [
              piped_apply "build_tree" [
                piped_apply "build_alphabet" [
                  print_string input
          ]]]]
        in
        black_box_compare
          (=) show_char_list
          (incorrect name)
          (wrap print) ()
          actual_behavior
          expected_behavior
      end;
      (* Check 2. *)
      inputs |> List.iter begin fun input ->
        let actual_tree =
          input
          |> build_alphabet
          |> build_tree
        in
        let actual_encoding_dictionary =
          actual_tree
          |> Solution.build_encoding_dictionary
        in
        let actual_encoded_input =
          input
          |> Solution.encode actual_encoding_dictionary
        in
        let actual_encoded_length =
          actual_encoded_input
          |> String.length
        in
        let expected_encoded_length = Solution.(
          let encoding_dictionary =
            input
            |> build_alphabet
            |> build_tree
            |> build_encoding_dictionary
          in
          input
          |> encode encoding_dictionary
          |> String.length
        ) in
        assert (expected_encoded_length <= actual_encoded_length);
        if expected_encoded_length < actual_encoded_length then begin
          let print_expr input =
            piped_apply "build_tree" [
              piped_apply "build_alphabet" [
                print_string input
              ]]
          in
          fail (
            incorrect name @
            R.Text "The following expression:" ::
            R.Break ::
            R.Code (wrap print_expr input) ::
            R.Break ::
            R.Text "yields the following tree, which is suboptimal:" ::
            R.Break ::
            R.Code (show_tree actual_tree) ::
            R.Break ::
            R.Text (sprintf
              "According to this tree, the input text \"%s\" \
               is encoded as the binary string %s, \
               whose length is %d bits, \
               whereas, by using another tree, \
               this input text can be encoded \
               as a binary string of only %d bits."
              input
              actual_encoded_input
              actual_encoded_length
              expected_encoded_length
            ) ::
            []
          )
        end
      end;
      correct name
    )))
  )

(* -------------------------------------------------------------------------- *)

(* Grading [build_encoding_dictionary]. *)

let trees =
  deepening Generate.all_trees_with_distinct_leaves 5

let test_build_encoding_dictionary () =
  section "Question 3" (
    test_value_1_in_context
      "build_encoding_dictionary" [%ty: tree -> encoding_dictionary]
      Solution.build_encoding_dictionary
      identity print_tree
      (* context: *) entries print_entries
      (=) show_char_string_list
      trees
  )

(* -------------------------------------------------------------------------- *)

(* Grading [find]. *)

let triples : (data * int * tree) list =
  trees |> flat_map (fun tree ->
    paths tree |> flat_map (fun path ->
      (path, 0, tree) ::
      (path ^ "garbage", 0, tree) ::
      ("garbage" ^ path, 7, tree) ::
      []
    )
  )

let test_find () =
  section "Question 4" (
    test_value_3
      "find" [%ty: data -> int -> tree -> char * int]
      Solution.find
      print_string print_int print_tree
      show_char_int (=)
      triples
  )

(* -------------------------------------------------------------------------- *)

(* Testing that a student function [f] produces binary data when applied to
   an argument [x]. *)

let test_binary_data msg f x show_f =
  let actual_behavior = T.result (fun () -> f x) in
  (* This is a modified [black_box_compare]. *)
  match actual_behavior with
  | Error TODO ->
      raise TODO
  | Error _ ->
      fail (
        msg @
        R.Text "The following expression:" ::
        R.Break ::
        R.Code (show_f x) ::
        R.Break ::
        show_actual_behavior show_string actual_behavior
        (* not explicitly said: raising an exception is invalid *)
      )
  | Ok data ->
      if not (is_binary_string data) then
        fail (
          msg @
          R.Text "The following expression:" ::
          R.Break ::
          R.Code (show_f x) ::
          R.Break ::
          show_actual_behavior show_string actual_behavior @
          R.Break ::
          R.Text "This is not binary data. \
                  No characters other than '0' and '1' must be used." ::
          []
        )

(* -------------------------------------------------------------------------- *)

(* Grading [write] and [read]. *)

let print_write tree =
  apply "write" [ parens (print_tree tree) ]

let show_write =
  wrap print_write

let test_write_read () =
  section "Question 5" (
    grab [%ty: tree -> data] "write" (fun write ->
    grab [%ty: data -> tree * int] "read" (fun read ->
    protect (fun () ->
      (* Check 0. Check that [write] does not crash and that its result is
         binary data. *)
      trees |> List.iter begin fun tree ->
        test_binary_data
          (incorrect "write")
          write tree
          show_write
      end;
      (* Check 1. Test the first component of the result. *)
      trees |> List.iter begin fun tree ->
        let actual_behavior =
          T.result (fun () ->
            fst (read (write tree))
          )
        and expected_behavior =
          Ok tree
        in
        let print () =
          piped_apply "fst" [
            piped_apply "read" [
              piped_apply "write" [
                print_tree tree
          ]]]
        in
        black_box_compare
          (=) show_tree
          something_is_wrong
          (wrap print) ()
          actual_behavior
          expected_behavior;
      end;
      (* Check 2. Test the second component of the result. *)
      trees |> List.iter begin fun tree ->
        let actual_behavior =
          T.result (fun () ->
            let data = write tree ^ "cookie" in
            let i = snd (read data) in
            String.sub data i 6
          )
        and expected_behavior =
          Ok "cookie"
        in
        let print () =
          let var = utf8string in
          elet "data" (
            infix_apply "^"
              (apply "write" [ parens (print_tree tree) ])
              (print_string "cookie")
          )(
            elet "i" (
              apply "snd" [ parens_apply "read" [ var "data" ]]
            )(
              apply "String.sub" [ var "data"; var "i"; print_int 6 ]
            )
          )
        in
        black_box_compare
          (=) show_string
          something_is_wrong
          (wrap print) ()
          actual_behavior
          expected_behavior
      end;
      correct2 "write" "read"
    )))
  )

(* -------------------------------------------------------------------------- *)

(* Grading [compress] and [decompress]. *)

let print_compress input =
  piped_apply "compress" [ print_string input ]

let print_decompress_compress input =
  piped_apply "decompress" [ print_compress input ]

let test_compress_decompress () =
  section "Question 6" (
    grab [%ty: text -> data] "compress" (fun compress ->
    grab [%ty: data -> text] "decompress" (fun decompress ->
    protect (fun () ->
      (* Check 0. Check that [compress] does not crash and that its result is
         binary data. *)
      inputs |> List.iter begin fun input ->
        test_binary_data
          (incorrect "compress")
          compress input
          (wrap print_compress)
      end;
      (* Check 1. The composition of [compress] and [decompress]
         should be the identity. *)
      inputs |> List.iter begin fun input ->
        let actual_behavior =
          T.result (fun () ->
            decompress (compress input)
          )
        and expected_behavior =
          Ok input
        in
        black_box_compare
          (=) show_string
          something_is_wrong
          (wrap print_decompress_compress) input
          actual_behavior
          expected_behavior
      end;
      (* Check 2. On long sentences, some compression should achieved. *)
      [ long_sentence ] |> List.iter begin fun input ->
        let input_length (* in bits *) = 8 * String.length input in
        let actual_result = compress input in
        let actual_result_length (* in bits *) = String.length actual_result in
        if not (actual_result_length < input_length) then
          fail (
            something_is_wrong @
            R.Text "The following expression:" ::
            R.Break ::
            R.Code (wrap print_compress input) ::
            R.Break ::
            R.Text (
              sprintf "produces a sequence of %d bits, \
                       whereas the original sentence occupies %d bits \
                       when represented as an ASCII string. \
                       No compression has been achieved! \
                       Perhaps your binary encoding of the dictionary \
                       is not compact enough?"
                actual_result_length
                input_length
            ) ::
            []
          )
      end;
      (* OK. *)
      correct2 "compress" "decompress"
    )))
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_build_alphabet() @
  test_build_tree() @
  test_build_encoding_dictionary() @
  test_find() @
  test_write_read() @
  test_compress_decompress() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
