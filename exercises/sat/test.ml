open Printf
let iter = List.iter
let map = List.map
let force = Lazy.force
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

let rec drop k xs =
  match k, xs with
  | 0, _
  | _, [] ->
      xs
  | _, x :: xs ->
      drop (k - 1) xs

let option_iter f o =
  match o with
  | None ->
      ()
  | Some x ->
      f x

let option_map f o =
  match o with
  | None ->
      None
  | Some x ->
      Some (f x)

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

  (* An indexed sum. [exists xs s] is the concatenation of the
     sequences [s x], where [x] ranges over the list [xs]. *)

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
      force tests |> List.iter (fun x ->
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
      force tests |> List.iter (fun ((x1, x2) as x) ->
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

(* A printer for pairs. *)

let print_pair print_x print_y (x, y) =
  tuple [ print_x x; print_y y ]

(* -------------------------------------------------------------------------- *)

(* Generators of formulae on [n] variables. *)

(* This generator generates a formula without going through the smart
   constructors. This formula can in fact be regarded as a syntactic
   description of an OCaml expression where each node represents a call
   to a smart constructor. *)

let sense : bool enum =
  Feat.finite [ false; true ]

let syntactic_formula (n : int) : formula enum =
  let const sense = FConst sense
  and conn (sense, (f1, f2)) = FConn (sense, f1, f2)
  and neg f = FNeg f
  and var x = FVar x in
  fix (fun formula ->
    Feat.(
      map const sense ++
      map var (finite (up 0 n)) ++
      pay (
        map conn (sense ** formula ** formula) ++
        map neg formula
      )
    )
  )

(* This generator generates a formula by going through our smart constructors.
   Therefore, the formula satisfies our invariant. *)

let formula (n : int) : formula enum =
  Feat.(Solution.(
    let conn (sense, (f1, f2)) = conn sense f1 f2 in
    (* The constants are used only outside the fixed point. There is no point
       in using them inside. *)
    map const sense ++
    fix (fun formula ->
      map var (finite (up 0 n)) ++
      pay (
        map conn (sense ** formula ** formula) ++
        map neg formula
      )
    )
  ))

(* This generator produces deeper (unbalanced) formulae. *)

let deep (n : int) : formula enum =
  Feat.(Solution.(
    let vars = map var (finite (up 0 n)) in
    let conn (sense, (f1, f2)) = conn sense f1 f2 in
    (* The constants are used only outside the fixed point. There is no point
       in using them inside. *)
    map const sense ++
    fix (fun formula ->
      vars ++
      pay (
        map conn (sense ** vars ** formula) ++
        map neg formula
      )
    )
  ))

(* Interpreting a formula as a tree of calls to the smart constructors. *)

module Interpret (X : sig
  val const: bool -> formula
  val conn: bool -> formula -> formula -> formula
  val neg: formula -> formula
  val var: var -> formula
end) = struct
  open X

  let rec interpret (f : formula) : formula =
    match f with
    | FConst sense ->
        const sense
    | FConn (sense, f1, f2) ->
        conn sense (interpret f1) (interpret f2)
    | FNeg f ->
        neg (interpret f)
    | FVar x ->
        var x

end

(* Printing a formula as a tree of calls to the smart constructors. *)

let rec print_tree f =
  match f with
  | FConst sense ->
      apply "const" [ print_bool sense ]
  | FConn (sense, f1, f2) ->
      apply "conn" [ print_bool sense; self f1; self f2 ]
  | FNeg f ->
      apply "neg" [ self f ]
  | FVar x ->
      apply "var" [ print_int x ]

and self f =
  parens (print_tree f)

let show_tree f =
  wrap print_tree f

(* Printing a formula as a formula. *)

let rec print_formula f =
  match f with
  | FConst sense ->
      construct "FConst" [ print_bool sense ]
  | FConn (sense, f1, f2) ->
      construct "FConn" [ print_bool sense; print_formula f1; print_formula f2 ]
  | FNeg f ->
      construct "FNeg" [ print_formula f ]
  | FVar x ->
      construct "FVar" [ print_int x ]

let parens_print_formula f =
  parens (print_formula f)

let show_formula f =
  wrap print_formula f

(* Printing a formula in human-readable syntax. *)

let rec pretty0 f =
  match f with
  | FConst sense ->
      utf8string (if sense then "true" else "false")
  | FNeg f ->
      utf8string "~" ^^ pretty0 f
  | FVar x ->
      utf8format "x%d" x
  | _ ->
      parens (pretty f)

and pretty_conj f =
  match f with
  | FConn (true, f1, f2) ->
      pretty_conj f1 ^^ utf8string " &" ^^ break 1 ^^ pretty_conj f2
  | _ ->
      pretty0 f

and pretty_disj f =
  match f with
  | FConn (false, f1, f2) ->
      pretty_disj f1 ^^ utf8string " |" ^^ break 1 ^^ pretty_disj f2
  | _ ->
      pretty0 f

and pretty f =
  match f with
  | FConn (false, _, _) ->
      pretty_disj f
  | FConn (true, _, _) ->
      pretty_conj f
  | _ ->
      pretty0 f

let show_pretty_formula =
  wrap pretty

(* Printing an environment. *)

let env_to_array n (env : env) : bool array =
  Array.init n env

let array_to_env (a : bool array) : env =
  Array.get a

let print_env n env =
  parens_apply "Array.get" [ print_array print_bool (env_to_array n env) ]

(* -------------------------------------------------------------------------- *)

(* Generating formulae directly in CNF form. *)

module CNFGen = struct

  open Feat

  type literal =
    var * bool

  type clause =
    literal list

  type clauses =
    clause list

  (* In the following, [n] is the number of variables in scope. *)

  let var n : var enum =
    finite (up 0 n)

  let literal n : literal enum =
    var n ** bool

  let clause n : clause enum =
    list (literal n)
      (* No mechanism is set up to avoid mentioning a variable twice
         inside a clause. *)

  let clauses n : clauses enum =
    list (clause n)

  (* Converting to the type [formula]. *)

  open Solution

  let convert_literal (x, p) =
    if p then var x else FNeg (var x)

  let convert_clause clause =
    List.fold_left (fun f lit ->
      disj f (convert_literal lit)
    ) falsity clause

  let convert_clauses clauses =
    List.fold_left (fun f clause ->
      conj f (convert_clause clause)
    ) truth clauses

  let cnf n : formula enum =
    map convert_clauses (clauses n)

end

(* -------------------------------------------------------------------------- *)

(* Testing the smart constructors. *)

let test_constructors () =
  section "Question 1" (
  grab [%ty: bool -> formula] "const" (fun const ->
  grab [%ty: bool -> formula -> formula -> formula] "conn" (fun conn ->
  grab [%ty: formula -> formula] "neg" (fun neg ->
  grab [%ty: var -> formula] "var" (fun var ->
    let module Candidate = Interpret(struct
      let const = const
      let conn = conn
      let neg = neg
      let var = var
    end) in
    let module Reference = Interpret(Solution) in
    let formulae = Feat.(sample 300 3 (syntactic_formula 1)) in
    protect (fun () ->
      formulae |> List.iter (fun f ->
        let actual_behavior = T.result (fun () -> Candidate.interpret f)
        and expected_behavior = T.result (fun () -> Reference.interpret f) in
        black_box_compare
          (=) show_formula
          something_is_wrong
          show_tree f
          actual_behavior
          expected_behavior
      );
      let message = [ R.Text "The smart constructors seem correct."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  )))))

(* -------------------------------------------------------------------------- *)

(* Testing [eval]. *)

(* [n] is the number of variables. *)

let test_eval () =
  let n = 1 in
    (* No need to test with more variables. *)
  let tests =
    lazy (
      let formulae = Feat.(sample 50 5 (formula n)) in
      let envs = [ (fun _ -> false); (fun _ -> true) ] in
      pairs envs formulae
    )
  in
  section "Question 2" (
  test_value_2
    "eval" [%ty: env -> formula -> bool] Solution.eval
    (print_env n)
    parens_print_formula
    show_bool (=)
    tests
  )

(* -------------------------------------------------------------------------- *)

(* Testing [satisfiable] and [valid]. *)

(* We could test [foreach_env] independently, but I am feeling lazy today. *)

let tests n s =
  (* [n] is the maximum number of variables. *)
  (* [s] is the maximum size. *)
  lazy (
    (up 0 (n+1)) |> flat_map (fun n ->
      let formulae = Feat.(sample 50 s (
        (* We use two generators, [formula] and [deep]. *)
        formula n ++ deep n
      )) in
      formulae |> map (fun f -> (n, f))
    )
  )

let test_satisfiable_valid () =
  section "Question 3" (
  let tests = tests 2 4 in
  test_value_2
    "satisfiable" [%ty: int -> formula -> bool] Solution.satisfiable
    print_int parens_print_formula
    show_bool (=)
    tests
  @
  test_value_2
    "valid" [%ty: int -> formula -> bool] Solution.valid
    print_int parens_print_formula
    show_bool (=)
    tests
  )

(* -------------------------------------------------------------------------- *)

(* Testing [CNF]. *)

(* [CNF] is a functor, so the incantation that is required in order to grab it
   is a little involved. *)

module type CNF_CONVERTER =
  functor (X : sig
    type clause
    val empty: clause
    val cons: bool -> var -> clause -> clause
    val new_var: unit -> var
    val new_clause: clause -> unit
  end) -> sig
    val cnf: formula -> unit
  end

module type CNF_STUDENT = sig
  module CNF : CNF_CONVERTER
end

module X (V : sig val n: int end) = struct
  open V
  type clause = formula
  let empty = Solution.falsity
  let lit s x = if s then Solution.var x else Solution.neg (Solution.var x)
  let cons s x f = Solution.disj (lit s x) f
  (* A counter of auxiliary variables, allocated
     above the original variables. *)
  let m = ref n
  let new_var () = postincrement m
  (* The conjunction of all clauses. *)
  let f = ref Solution.truth
  let new_clause c =
    f := Solution.conj !f c
  (* Retrieving the final number of auxiliary variables and the final
     formula in conjunctive normal form. *)
  let retrieve () = !m, !f
end

let present_formula f =
  R.Break ::
  R.Code (show_formula f) ::
  R.Break ::
  R.Text "This formula can also be displayed as follows:" ::
  R.Break ::
  R.Code (show_pretty_formula f) ::
  R.Break ::
  []

let extract_result f actual_behavior =
  if actual_behavior = Error TODO then
    raise TODO
  else match actual_behavior with
  | Ok v ->
      v
  | Error e ->
      fail (
        R.Text "Something is wrong. Consider this formula:" ::
        present_formula f @
        R.Text "While attempting to convert this formula to conjunctive \
                normal form, the following exception was raised:" ::
        R.Output (Printexc.to_string e) ::
        []
      )

let something_is_wrong n f =
  R.Text (sprintf
    "Something is wrong. Consider this formula (of %d variable%s):"
    n (if n > 1 then "s" else "")
  ) ::
  present_formula f

let converting_yields n' f' =
  R.Text (sprintf
    "Converting this formula to conjunctive normal form \
     yields the following new formula (of %d variable%s):"
    n' (if n' > 1 then "s" else "")
  ) ::
  present_formula f'

let show_var x =
  sprintf "x%d" x (* see also [pretty0] *)

let show_literal (x, p) =
  if p then show_var x else "~" ^ show_var x

let show_assignment (n : int) (env : var -> bool) =
  let b = Buffer.create 32 in
  for x = 0 to n - 1 do
    Buffer.add_string b (show_literal (x, env x));
    if x < n - 1 then
      Buffer.add_string b " & "
  done;
  Buffer.contents b

let present_assignment n env =
  R.Break ::
  R.Code (show_assignment n env) ::
  R.Break ::
  []

let test_cnf () =
  section "Question 4" (
  T.test_student_code [%ty: (module CNF_STUDENT)] (fun student ->
    let module Student = (val student: CNF_STUDENT) in
    protect (fun () ->
      tests 3 5 |> force |> List.iter (fun (n, f) ->
        (* Apply the student's [CNF] functor to a fresh instance of [X]
           so as to be able to record the result of CNF conversion. *)
        let actual_behavior = T.result (fun () ->
          let module X = X(struct let n = n end) in
          let module C = Student.CNF(X) in
          C.cnf f;
          X.retrieve()
        ) in
        (* If the student code throws an exception, report it. Otherwise,
           we get a formula [f'] in conjunctive normal form with [m] new
           existentially quantified variables. *)
        let m, f' = extract_result f actual_behavior in
        (* We must check whether [f] and [exists m.f'] are equivalent. *)
        (* Note that this property is stronger than just stating that
           [f] and [f'] are equisatisfiable. Indeed, taking [m = 0],
           and taking [f'] to be a copy of [f] whose variables have
           been permuted, obviously [f] and [f'] are equisatisfiable,
           yet they are in general not equivalent. *)
        (* Assuming that [n] and [m] are small, we can check the desired
           property in a naive way, by enumerating all assignments. *)
        Solution.foreach_env n (fun envn ->
          let b = Solution.eval envn f in
          let satisfiable' = ref false in
          Solution.foreach_env m (fun envm ->
            let env' x = if x < n then envn x else envm (x - n) in
            let b' = Solution.eval env' f' in
            satisfiable' := !satisfiable' || b';
            if (not b) && b' then
              fail (
                something_is_wrong n f @
                converting_yields (n + m) f' @
                R.Text "Unfortunately, these formulae do not have the desired relationship." ::
                R.Text "The original formula is not satisfied by this assignment:" ::
                present_assignment n envn @
                R.Text "whereas the new formula is satisfied by this extended assignment:" ::
                present_assignment (n + m) env' @
                []
              )
          );
          if b && (not !satisfiable') then fail (
            something_is_wrong n f @
            converting_yields (n + m) f' @
            R.Text "Unfortunately, these formulae do not have the desired relationship." ::
            R.Text "The original formula is satisfied by this assignment:" ::
            present_assignment n envn @
            R.Text "whereas the new formula is not satisfied by any extension of this assignment." ::
            []
          )
        )
      );
      correct "CNF"
    )
  ))

(* -------------------------------------------------------------------------- *)

(* Testing [intersect]. *)

let test_intersect () =
  let lists = [
    [];
    [1];
    [2];
    [3];
    [1;2];
    [1;3];
    [2;3];
    [1;2;3];
  ] in
  let tests = lazy (
    pairs lists lists
  ) in
  section "Question 5" (
    test_value_2
      "intersect" [%ty: int list -> int list -> bool] Solution.intersect
      print_list_int
      print_list_int
      show_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Testing [Trail]. *)

(* This is a mini-exercise in itself. *)

module type TRAIL =
  sig
    val push: (unit -> unit) -> unit
    type checkpoint
    val record: unit -> checkpoint
    val revert: checkpoint -> unit
  end

module type TRAIL_STUDENT = sig
  module Trail : functor () -> TRAIL
end

module TestingTrail = struct

type instruction =
    (* Write a new value the reference [s] and push an undo action. *)
  | ISetPush
    (* Bind a new checkpoint variable. *)
  | IRecord
    (* Revert to a previously bound checkpoint. *)
    (* Invalidate all strictly newer checkpoints. *)
  | IRevert of int (* index *)
    (* Observe the current state of [s]. *)
  | IObserve of int ref (* expected value *)

type instructions =
  instruction list

let no_observation =
  -42

let freshen i =
  match i with
  | ISetPush
  | IRecord
  | IRevert _ ->
      i
  | IObserve _ ->
      IObserve (ref no_observation)

let freshen is =
  map freshen is

module G = struct

  open Feat

  (* Generation of instructions. *)

  (* [senv] is the number of checkpoint variables in scope. *)

  let instructions : int -> instructions enum =
    fix2 (fun instructions senv ->
      just [] ++ pay (
        map (fun is -> ISetPush :: is) (instructions senv) ++
        map (fun is -> IRecord :: is) (instructions (senv + 1)) ++
        exists (up 0 senv) (fun x ->
          map
            (fun is -> IRevert x :: is)
            (instructions (senv - x))
        ) ++
        map (fun is -> IObserve (ref no_observation) :: is) (instructions senv)
      )
    )

  let instructions : instructions enum =
    instructions 0

end

module I (Trail : TRAIL) = struct

  (* [c] counts how the [ISetPush] instructions executed so far,
     and is used to generate the value written by the next such
     instruction. *)

  (* [s] is a global state. *)

  let rec interpret env (c : int) (s : int ref) (is : instruction list) =
    match is with
    | [] ->
        ()
    | ISetPush :: is ->
        let current = !s in
        s := c;
        Trail.push (fun () -> s := current);
        interpret env (c+1) s is
    | IRecord :: is ->
        let checkpoint = Trail.record() in
        interpret (checkpoint :: env) c s is
    | IRevert x :: is ->
        let checkpoint = List.nth env x in
        Trail.revert checkpoint;
        interpret (drop x env) c s is
    | IObserve o :: is ->
        let n = !s in
        assert (n <> no_observation);
        if !o = no_observation then
          (* In the reference run, record an observation. *)
          o := n
        else
          (* In the candidate run, compare the previously recorded
             observation with what we get here. *)
          if !o <> n then
            raise (ExpectedGot (!o, n));
        interpret env c s is

  let interpret s is =
    interpret [] 0 s is

end

let init =
  -1 (* initial value of the global state [s] *)

(* A printer for instruction sequences. *)

(* I am really not proud of writing such complex and fragile code. *)

let show_instructions is =
  let b = Buffer.create 32 in
  let out format = Printf.bprintf b format in
  out
    "let module Trail = Trail() in\n\
     let s = ref (%d) in\n" init;
  let c = ref 0 in
  let senv = ref 0 in
  is |> List.iter (fun i ->
    match i with
    | ISetPush ->
        out
          "(* Set [s] to %d and push an undo action. *)\n\
           let x = !s in s := %d; Trail.push (fun () -> s := x);\n"
          !c !c;
        incr c
    | IRecord ->
        out
          "(* Take a checkpoint. *)\n\
           let checkpoint%d = Trail.record() in\n"
          !senv;
        incr senv
    | IRevert x ->
        assert (0 <= x && x < !senv);
        out
          "(* Go back to this checkpoint. *)\n\
           Trail.revert checkpoint%d;\n"
          (!senv - x - 1);
        senv := !senv - x
    | IObserve o ->
        let o = !o in
        assert (o <> no_observation);
        out
          "(* Check the value of [s]. *)\n\
           if %d <> !s then raise (ExpectedGot (%d, !s));\n"
          o o
  );
  out "()";
  Buffer.contents b

end

let test_trail () =
  let open TestingTrail in
  section "Question 6" (
  T.test_student_code [%ty: (module TRAIL_STUDENT)] (fun student ->
    let module Student = (val student: TRAIL_STUDENT) in
    let tests : instructions list = Feat.sample 700 5 G.instructions in
    protect (fun () ->
      tests |> List.iter (fun is ->
        let is = freshen is in
        (* Apply the solution's [Trail] functor. *)
        let module Foo = Solution.Trail() in
        let module Reference = I(Foo) in
        (* Run the reference implementation and annotate the instruction
           sequence [is] with expected observations. *)
        Reference.interpret (ref init) is;
        let actual_behavior = T.result (fun () ->
          (* Apply the student's [Trail] functor. Run the candidate
             implementation and compare this run against the expected
             observations obtained above. *)
          let module Bar = Student.Trail() in
          let module Candidate = I(Bar) in
          Candidate.interpret (ref init) is
        ) in
        (* Report errors raised either by the student's code or by our
           comparison of expected versus actual observations. *)
        if actual_behavior = Error TODO then
          raise TODO
        else match actual_behavior with
        | Ok _ ->
            ()
        | Error e ->
            fail (
              R.Text "Something is wrong. Executing the following instruction sequence:" ::
              R.Code (show_instructions is) ::
              R.Text "caused the following exception to be raised:" ::
              R.Output (Printexc.to_string e) ::
              []
            )
      );
      correct "Trail"
    )
  ))

(* -------------------------------------------------------------------------- *)

(* Testing [VarSet]. *)

(* This is another mini-exercise in itself. *)

(* Because the specification of [pick] is nondeterministic, we cannot compare
   the student's solution against our solution. We must compare the student's
   solution against the specification. *)

module type VARSET =
  sig
    val mem: var -> bool
    val add: var -> unit
    val remove: var -> unit
    val pick: unit -> var option
  end

module type VARSET_STUDENT = sig
  module VarSet : functor () -> VARSET
end

module TestingVarSet = struct

  module IntSet =
    Set.Make(struct type t = int let compare = compare end)

  (* Because our [add] and [remove] instructions have preconditions
     (the element must not / must be in the set), it is difficult
     to directly enumerate sequences of valid instructions. We
     could use random generation instead of enumeration. Instead,
     we choose to enumerate sequences of pre-instructions, which
     we later transform into sequences of instructions. *)

  (* An index of an element in a set. E.g., in the set {1,2,5},
     the three elements have indices 0, 1, 2. *)

  type index = int

  type pre_instruction =
  | PreMemPresent of index
  | PreMemAbsent
  | PreAdd
  | PreRemove of index
  | PrePick

  type pre_instructions =
    pre_instruction list

  type instruction =
  | Mem of var * bool (* element, expected result *)
  | Add of var        (* element *)
  | Remove of var     (* element *)
  | Pick

  type instructions =
    instruction list

  module G = struct

    open Feat

    (* Enumeration of sequences of pre-instructions. The parameter [n] is the
       number of elements in the set so far. (We do not keep track of the
       values of the elements.) *)

    let pre_instructions : int -> pre_instructions enum =
      fix2 (fun pre_instructions n ->
        just [] ++ pay (
          let index = interval 0 n in
          map (fun (x, is) -> PreMemPresent x :: is) (index ** pre_instructions n) ++
          map (fun is -> PreMemAbsent :: is) (pre_instructions n) ++
          map (fun is -> PreAdd :: is) (pre_instructions (n + 1)) ++
          map (fun (x, is) -> PreRemove x :: is) (index ** pre_instructions (n - 1)) ++
          map (fun is -> PrePick :: is) (pre_instructions n)
        )
      )

    let pre_instructions : pre_instructions enum =
      pre_instructions 0

    (* Converting pre-instructions to instructions. *)

    let nth (xs : IntSet.t) (x : int) : int =
      try
        List.nth (IntSet.elements xs) x
      with Failure _ ->
        assert false

    let rec choose_outside (xs : IntSet.t) (x : int) : int =
      if IntSet.mem x xs then
        choose_outside xs (x + Random.int 10)
      else
        x

    let choose_outside xs =
      choose_outside xs 0

    let rec convert (xs : IntSet.t) (is : pre_instructions) : instructions =
      match is with
      | [] ->
          []
      | PreMemPresent x :: is ->
          Mem (nth xs x, true) :: convert xs is
      | PreMemAbsent :: is ->
          Mem (choose_outside xs, false) :: convert xs is
      | PreAdd :: is ->
          let x = choose_outside xs in
          let xs = IntSet.add x xs in
          Add x :: convert xs is
      | PreRemove x :: is ->
          let x = nth xs x in
          let xs = IntSet.remove x xs in
          Remove x :: convert xs is
      | PrePick :: is ->
          Pick :: convert xs is

    let convert : pre_instructions -> instructions =
      convert IntSet.empty

    let instructions : instructions enum =
      map convert pre_instructions

  end

  module I (VarSet : VARSET) = struct

    (* [xs] is our idea of what variables are presently in the set. *)

    let rec interpret (xs : IntSet.t) (is : instructions) =
      match is with
      | [] ->
          ()
      | Mem (x, expected) :: is ->
          if false then assert (expected = IntSet.mem x xs);
          let observed = VarSet.mem x in
          if not (expected = observed) then
            raise (ExpectedGotB (expected, observed));
          interpret xs is
      | Add x :: is ->
          let xs = IntSet.add x xs
          and () = VarSet.add x in
          interpret xs is
      | Remove x :: is ->
          let xs = IntSet.remove x xs
          and () = VarSet.remove x in
          interpret xs is
      | Pick :: is ->
          let observed = VarSet.pick() in
          begin match IntSet.is_empty xs, observed with
            | true, None ->
                ()
            | false, Some x ->
                if not (IntSet.mem x xs) then
                  raise (NotASetElement x)
            | true, Some x ->
                raise (ExpectedNoneGotSome x)
            | false, None ->
                raise (ExpectedSomeGotNone)
          end;
          interpret xs is

    let interpret (is : instructions) =
      interpret IntSet.empty is

  end

  (* A printer for instruction sequences. *)

  let show_mem (xs : IntSet.t) : string =
    IntSet.elements xs
    |> List.map (fun x -> sprintf "x = %d" x)
    |> String.concat " || "

  let show_instructions (is : instructions) : string =
    let b = Buffer.create 32 in
    let out format = Printf.bprintf b format in
    out "let module V = VarSet() in\n";
    let xs = ref IntSet.empty in
    is |> List.iter (fun i ->
      match i with
      | Mem (x, expected) ->
          assert (expected = IntSet.mem x !xs);
          out
            "let expected = %b\n\
             and observed = V.mem %d in\n\
             if not (expected = observed) then\n\
               raise (ExpectedGotB (expected, observed));\n"
            expected x
      | Add x ->
          out "V.add %d;\n" x;
          xs := IntSet.add x !xs
      | Remove x ->
          out "V.remove %d;\n" x;
          xs := IntSet.remove x !xs
      | Pick ->
          out "let observed = V.pick() in\n";
          if IntSet.is_empty !xs then
            out "\
            begin match observed with\n\
            | None   -> ()\n\
            | Some x -> raise (ExpectedNoneGotSome x)\n\
            end\n"
          else
            out "\
            begin match observed with\n\
            | None   -> raise (ExpectedSomeGotNone)\n\
            | Some x -> if not (%s) then raise (NotASetElement x)\n\
            end\n"
              (show_mem !xs)
    );
    out "()";
    Buffer.contents b

end

let print_ad_hoc_exception e =
  match e with
  | ExpectedGotB (expected, obtained) ->
      sprintf "ExpectedGotB (%b, %b)" expected obtained
  | _ ->
      (* [Printexc.to_string] does not have type information
         and assumes that every argument is an integer; which
         is why the special case above is needed. *)
      Printexc.to_string e

let test_varset () =
  let open TestingVarSet in
  section "Question 7" (
  T.test_student_code [%ty: (module VARSET_STUDENT)] (fun student ->
    let module Student = (val student: VARSET_STUDENT) in
    let tests = Feat.sample 700 5 G.instructions in
    protect (fun () ->
      tests |> List.iter (fun is ->
        let actual_behavior = T.result (fun () ->
          (* Apply the student's [VarSet] functor. *)
          let module Bar = Student.VarSet() in
          let module Candidate = I(Bar) in
          Candidate.interpret is
        ) in
        (* Report errors raised either by the student's code or by our test. *)
        if actual_behavior = Error TODO then
          raise TODO
        else match actual_behavior with
        | Ok _ ->
            ()
        | Error e ->
            fail (
              R.Text "Something is wrong. Executing the following instruction sequence:" ::
              R.Code (show_instructions is) ::
              R.Text "caused the following exception to be raised:" ::
              R.Output (print_ad_hoc_exception e) ::
              []
            )
      );
      correct "VarSet"
    )
  ))

(* -------------------------------------------------------------------------- *)

(* Testing the SAT solver. *)

(* In fact, we test the composition of the CNF conversion (which has
   been tested in isolation earlier) and the SAT solver. Perhaps it
   would be preferable to first test the SAT solver in isolation. To
   compensate for this shortcoming, the first formulae that we submit
   to the solver are formulae in CNF form, so (in principle) they
   should be left untouched by the student's CNF conversion code. *)

module type SAT_SOLVER =
  functor () -> sig
    val solve: int -> formula -> (var -> bool) option
  end

module type SAT_STUDENT = sig
  module SAT : SAT_SOLVER
end

let extract_result n f actual_behavior =
  if actual_behavior = Error TODO then
    raise TODO
  else match actual_behavior with
  | Ok v ->
      v
  | Error e ->
      fail (
        something_is_wrong n f @
        R.Text "While attempting to solve this formula, \
                the following exception was raised:" ::
        R.Output (Printexc.to_string e) ::
        []
      )

let test_solve () =
  section "Question 8" (
  T.test_student_code [%ty: (module SAT_STUDENT)] (fun student ->
    let module Student = (val student: SAT_STUDENT) in
    protect (fun () ->
      let cnf_formulae s (* size *) n (* vars *) =
        Feat.sample 1000 s (CNFGen.cnf n) |> map (fun f -> (n, f))
      in
      let formulae =
        cnf_formulae 2 0 @
        cnf_formulae 2 1 @
        cnf_formulae 2 2 @
        cnf_formulae 9 3 @
        cnf_formulae 7 4 @
        force (tests 2 5) @
        force (tests 3 7)
      in
      formulae |> List.iter (fun (n, f) ->
        (* Apply the student's [SAT] functor. Then, invoke [solve],
           and convert the result to an array of Booleans, so all
           calls to student functions are protected by [T.result]. *)
        let actual_behavior = T.result (fun () ->
          let module S = Student.SAT() in
          S.solve n f |> option_map (Array.init n)
        ) in
        (* If the student code throws an exception, report it. Otherwise,
           we get a result. *)
        let outcome : bool array option = extract_result n f actual_behavior in
        (* We must now check whether this outcome is correct. *)
        match outcome with
        | None ->
            (* The student claims that this formula is unsatisfiable. Check
               whether this is true by using our fastest SAT solver at hand,
               namely our own solution. (We could propose a faster solver if
               needed.) *)
            let module S = Solution.SAT() in
            let oenv = S.solve n f in
            begin match oenv with
            | Some env ->
                (* We have found a satisfying assignment! *)
                assert (Solution.eval env f);
                fail (
                  something_is_wrong n f @
                  R.Text "Your solver claims that this formula is \
                          unsatisfiable. Yet the following assignment \
                          satisfies it:" ::
                  present_assignment n env @
                  []
                )
            | None ->
                (* Neither the student nor us have found a satisfying assignment. *)
                (* We should be convinced, but we run one more (slow) sanity check. *)
                assert (not (Solution.satisfiable n f))
            end
        | Some (env : bool array) ->
            (* The student claims that [env] is a satisfying assignment. *)
            let env : var -> bool = Array.get env in
            if Solution.eval env f then
              (* The student is right. Our own solver should find
                 a satisfying assignment, too. *)
              assert (let module S = Solution.SAT() in S.solve n f <> None)
            else
              (* The student's proposed satisfying assignment is invalid! *)
              fail (
                something_is_wrong n f @
                R.Text "Your solver claims that this formula \
                        is satisfied by the following assignment:" ::
                present_assignment n env @
                R.Text "However, this is not the case. Under this \
                        assignment, this formula evaluates to false." ::
                []
              )
      );
      correct "SAT"
    )
  ))

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_constructors() -@>
  test_eval -@>
  test_satisfiable_valid -@>
  test_cnf -@>
  test_intersect -@>
  test_trail -@>
  test_varset -@>
  test_solve -@>
  fun () -> []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
