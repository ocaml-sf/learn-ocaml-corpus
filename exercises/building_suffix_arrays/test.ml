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

let block doc =
  nest 2 (break 0 ^^ doc) ^^ break 0

let parens doc =
  utf8string "(" ^^ block doc ^^ utf8string ")"

let flat_space =
  ifflat space empty

let brackets doc =
  utf8string "[" ^^ block (flat_space ^^ doc ^^ flat_space) ^^ utf8string "]"

let ocaml_array_brackets doc =
  utf8string "[|" ^^ block (flat_space ^^ doc ^^ flat_space) ^^ utf8string "|]"

let tuple docs =
  group (parens (commas docs))

let list docs =
  group (brackets (semis docs))

let array docs =
  group (ocaml_array_brackets (semis docs))

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

let def x e1 e2 =
  group (
    utf8string ("let " ^ x ^ " =") ^^
    nest 2 (break 1 ^^ e1) ^^ break 1 ^^
    utf8string "in"
  ) ^^ hardline ^^
  e2

let wrap (print : 'a -> document) : 'a -> string =
  fun x -> pretty 70 (group (print x))

(* -------------------------------------------------------------------------- *)

(* An implementation of symbolic sequences. *)

module SymSeq = struct

  type _ seq =
  | Empty    : 'a seq
  | Singleton: 'a -> 'a seq
  | Interval : int * int -> int seq
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
    | Interval (b, c) ->
        c - b
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

  let interval b c =
    if b < c then
      Interval (b, c)
    else
      empty

  let check length =
    assert (length >= 0); (* if this fails, an overflow has occurred *)
    length

  let sum s1 s2 =
    if is_empty s1 then s2
    else if is_empty s2 then s1
    else Sum (check (length s1 + length s2), s1, s2)

  let bigsum ss =
    List.fold_left sum empty ss

  let exists (xs : 'a list) (s : 'a -> 'b seq) : 'b seq =
    bigsum (List.map s xs)

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
      | Interval (b, c) ->
          if 0 <= i && b + i < c then b + i else raise OutOfBounds
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
      | Interval (b, c) ->
          for i = b to c-1 do
            k i
          done
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

(* A memoization combinator. *)

let memoize (f : 'a -> 'b) : 'a -> 'b =
  let table = Hashtbl.create 32 in
  let f x =
    try
      Hashtbl.find table x
    with Not_found ->
      let y = f x in
      Hashtbl.add table x y;
      y
  in
  f

let memoize2 (f : 'a -> 'b -> 'c) : 'a -> 'b -> 'c =
  curry (memoize (uncurry f))

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

  let interval b c : int enum =
    enum (SymSeq.interval b c)

  let pay (enum : 'a enum) : 'a enum =
    fun s ->
      if s = 0 then SymSeq.empty else enum (s-1)

  let sum (enum1 : 'a enum) (enum2 : 'a enum) : 'a enum =
    fun s ->
      SymSeq.sum (enum1 s) (enum2 s)

  let ( ++ ) =
    sum

  let exists (xs : 'a list) (enum : 'a -> 'b enum) : 'b enum =
    fun s ->
      SymSeq.exists xs (fun x -> enum x s)

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

let test_value_2_in_context name ty reference context printx1 printx2 showy eqy tests =
  grab ty name (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2) as x) ->
        let actual_behavior = T.result (fun () -> context candidate x1 x2)
        and expected_behavior = T.result (fun () -> context reference x1 x2) in
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

let test_value_2 name ty reference printx1 printx2 showy eqy tests =
  let context f x1 x2 = f x1 x2 in
  test_value_2_in_context name ty reference context printx1 printx2 showy eqy tests

let test_value_3_in_context name ty reference context printx1 printx2 printx3 showy eqy tests =
  grab ty name (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2, x3) as x) ->
        let actual_behavior = T.result (fun () -> context candidate x1 x2 x3)
        and expected_behavior = T.result (fun () -> context reference x1 x2 x3) in
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

let test_value_3 name ty reference printx1 printx2 printx3 showy eqy tests =
  let context f x1 x2 x3 = f x1 x2 x3 in
  test_value_3_in_context name ty reference context printx1 printx2 printx3 showy eqy tests

(* -------------------------------------------------------------------------- *)

(* Array utilities. *)

let truncate k (a : 'a array) : 'a array =
  let n = Array.length a in
  let k = min k n in
  if k = 0 then [||]
  else
    let b = Array.make k a.(0) in
    Array.blit a 0 b 0 k;
    b

let perturb (a : 'a array) : 'a array =
  let b = Array.copy a in
  let n = Array.length a in
  if n = 0 then
    a
  else
    let i = Random.int n
    and j = Random.int n in
    b.(i) <- b.(j);
    b

let swap xs i j =
  if i <> j then
    let x = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- x

let shuffle (xs : 'a array) : unit =
  let n = Array.length xs in
  for i = n downto 2 do
    swap xs (i-1) (Random.int i)
  done

let shuffle (a : 'a array) : 'a array =
  let b = Array.copy a in
  shuffle b;
  b

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

let show_int i =
  if i = max_int then
    "max_int"
  else if i = -max_int then
    "-max_int"
  else
    sprintf "%d" i

let print_int i =
  utf8string (show_int i)

let show_atomic_int i =
  if i >= 0 then
    show_int i
  else
    sprintf "(%s)" (show_int i)

let print_atomic_int i =
  utf8string (show_atomic_int i)

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
  array (map print_element (Array.to_list a))

(* A printer for lists. *)

let print_list print_element xs =
  list (map print_element xs)

let print_list_int =
  print_list print_int

let show_list_int =
  wrap print_list_int

(* A printer for pairs. *)

let print_pair print_x print_y (x, y) =
  tuple [ print_x x; print_y y ]

(* More printers. *)

let print_array_bool =
  print_array print_bool

let show_array_bool =
  wrap print_array_bool

let print_array_int =
  print_array print_int

let show_array_int =
  wrap print_array_int

let print_array_string =
  print_array print_string

let show_array_string =
  wrap print_array_string

(* -------------------------------------------------------------------------- *)

(* Grading [is_sorted]. *)

(* We currently do not check that [is_sorted] is polymorphic, but we could. *)

let lists : int list list =
  Feat.(sample 50 5 (list (finite (up 0 10))))

let arrays : int array list =
  map Array.of_list lists

let leq (x : int) (y : int) =
  x <= y

let geq (x : int) (y : int) =
  x >= y

let tests =
  let pair x y = (x, y) in
  map (pair leq) arrays @
  map (pair geq) arrays

let custom_print_leq f =
  if f == leq then
    utf8string "(<=)"
  else if f == geq then
    utf8string "(>=)"
  else
    assert false

let test_is_sorted () =
  section "Question 1" (
    test_value_2 "is_sorted" [%ty: (int -> int -> bool) -> int array -> bool]
      Solution.is_sorted
      custom_print_leq
      print_array_int
      show_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Grading [is_permutation]. *)

let lists : int list list =
  Feat.(sample 50 4 (list (finite (up 0 4))))

let arrays : int array list =
  map Array.of_list lists

let test_is_permutation () =
  section "Question 2" (
    test_value_1 "is_permutation" [%ty: int array -> bool]
      Solution.is_permutation
      (print_array print_int)
      show_bool (=)
      arrays
  )

(* -------------------------------------------------------------------------- *)

(* Grading [leq_suffix_suffix]. *)

let a_few_strings = [
  "car";
  "";
  "a";
  "mississipi";
]

let tests =
  a_few_strings |> flat_map (fun s ->
    let n = String.length s in
    let suffixes = up 0 (n+1) in
    suffixes |> flat_map (fun i ->
      suffixes |> flat_map (fun j ->
        [s, i, j]
      )
    )
  )

let test_leq_suffix_suffix () =
  section "Question 3" (
    test_value_3 "leq_suffix_suffix"
      [%ty: string -> suffix -> suffix -> bool]
      Solution.leq_suffix_suffix
      print_string
      print_int
      print_int
      show_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Grading [is_suffix_array]. *)

let positive_tests =
  a_few_strings |> map (fun s ->
    s, Solution.suffix_sort s
  )

let negative_tests =
  positive_tests |> flat_map (fun (s, a) ->
    let n = String.length s in
    (s, Array.append a [|n|]) ::
    (s, truncate (n/2) a) ::
    (s, shuffle a) ::
    (s, perturb a) ::
    (s, init n (fun i -> i)) ::
    []
  )

let test_is_suffix_array () =
  section "Question 4" (
    test_value_2 "is_suffix_array" [%ty: string -> suffix array -> bool]
      Solution.is_suffix_array
      print_string
      (print_array print_int)
      show_bool (=)
      (positive_tests @ negative_tests)
  )

(* -------------------------------------------------------------------------- *)

(* Grading [naive_suffix_sort]. *)

let short_strings =
  a_few_strings @ [
    "baby";
    "leonard";
    "sheldon";
    "aardvark";
    "abracadabrant";
]

let test_naive_suffix_sort () =
  section "Question 5" (
    test_value_1 "naive_suffix_sort" [%ty: string -> suffix array]
      Solution.suffix_sort
      print_string
      show_array_int (=)
      short_strings
  )

(* -------------------------------------------------------------------------- *)

(* Testing [pigeonhole_sort]. *)

(* Because we have required the sorting algorithm to be stable,
   its specification is deterministic. Therefore we can easily
   compare the candidate and the reference. *)

type key =
  | StringLength
  | StringFirst

let apply key s =
  match key with
  | StringLength ->
      String.length s
  | StringFirst ->
      Char.code s.[0]

let print_key key =
  match key with
  | StringLength ->
      utf8string "String.length"
  | StringFirst ->
      utf8string "(fun s -> Char.code s.[0])"

let strings =
  (* An array of strings of length at most 15: *)
  [|
    "hello";
    "obituary";
    "bob";
    "copycat";
    "hamster";
    "cinematographer";
    "cinema";
    "car";
    "bit";
    "car";
  |]

let tests = [
  16, StringLength, strings;
  256, StringFirst, strings;
  16, StringLength, [||];
  16, StringLength, [| "lonely" |];
]

let test_pigeonhole_sort () =
  section "Question 6" (
    (* The function does not return a result; it performs a side effect
       on its third argument. *)
    let context sort m key a =
      let b = Array.copy a in
      sort m (apply key) b;
      b
    in
    test_value_3_in_context "pigeonhole_sort"
      [%ty: int -> (string -> int) -> string array -> unit]
      Solution.pigeonhole_sort
      context
      print_int print_key print_array_string
      show_array_string (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Grading [beginning_of_bucket]. *)

let tests =
  List.map (fun (m, key, a) ->
    let a = Array.copy a in
    Solution.pigeonhole_sort m (apply key) a;
    key, a
  ) tests

let test_beginning_of_bucket () =
  section "Question 7" (
    let context begining_of_bucket key a =
      begining_of_bucket (apply key) a
    in
    test_value_2_in_context "beginning_of_bucket"
      [%ty: (string -> int) -> string array -> bool array]
      Solution.beginning_of_bucket
      context
      print_key print_array_string
      show_array_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Grading [suffix_sort]. *)

(* Same grader as [naive_suffix_sort]. However, since the efficiency
   of [suffix_sort] is supposed to be good, we can (and must) test it
   on longer strings. *)

(* An array of 100 random words. *)

let words = [|
  "worried";
  "queue";
  "answer";
  "berserk";
  "boorish";
  "moan";
  "prevent";
  "unusual";
  "obscene";
  "switch";
  "friendly";
  "serious";
  "prick";
  "cynical";
  "trap";
  "petite";
  "foamy";
  "trashy";
  "succeed";
  "ludicrous";
  "string";
  "obsolete";
  "cream";
  "stiff";
  "conscious";
  "decorate";
  "fireman";
  "husky";
  "force";
  "endurable";
  "downtown";
  "thing";
  "receptive";
  "faded";
  "cemetery";
  "colorful";
  "aspiring";
  "tremendous";
  "wing";
  "shape";
  "melt";
  "hypnotic";
  "branch";
  "leg";
  "point";
  "neat";
  "accidental";
  "deserted";
  "ground";
  "rat";
  "tip";
  "ski";
  "extend";
  "friend";
  "scold";
  "dad";
  "lackadaisical";
  "mend";
  "resonant";
  "beds";
  "uneven";
  "stitch";
  "voyage";
  "knife";
  "haircut";
  "subsequent";
  "hot";
  "functional";
  "eggs";
  "violet";
  "teeny-tiny";
  "hurried";
  "calm";
  "class";
  "wakeful";
  "hateful";
  "encourage";
  "decisive";
  "amusement";
  "duck";
  "frantic";
  "frightened";
  "allow";
  "warm";
  "sincere";
  "wrathful";
  "light";
  "quilt";
  "rigid";
  "tongue";
  "ad hoc";
  "foolish";
  "breath";
  "abstracted";
  "word";
  "red";
  "spill";
  "gray";
  "typical";
  "determined";
|]

let random_word () : string =
  words.(Random.int (Array.length words))

let random_word_sequence k : string =
  Array.init k (fun _i -> random_word())
  |> Array.to_list
  |> String.concat " "

let max_num_words =
  65536

let rec long_random_strings k : string list =
  if k >= max_num_words then
    []
  else
    random_word_sequence k ::
    long_random_strings (4*k)

let test_suffix_sort () =
  section "Question 8" (
    test_value_1 "suffix_sort" [%ty: string -> suffix array]
      Solution.suffix_sort
      print_string
      show_array_int (=)
      (short_strings @ long_random_strings 1)
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_is_sorted() -@>
  test_is_permutation -@>
  test_leq_suffix_suffix -@>
  test_is_suffix_array -@>
  test_naive_suffix_sort -@>
  test_pigeonhole_sort -@>
  test_beginning_of_bucket -@>
  test_suffix_sort -@>
  (fun () -> [])

let () =
  T.set_result (T.ast_sanity_check code_ast report)
