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

(* This variant of [black_box_compare] is used to compare two behaviors
   in the general case where they are expected to be equal but neither
   of them is the "reference" behavior. *)

let expect_equal_behavior
  (* Value equality and display, used to compare and show results. *)
  eq_value show_value
  (* The beginning of the error message.
     E.g. [something_is_wrong] or [incorrect name]. *)
  announcement
  (* Expression display. *)
  show_expr1 expr1
  show_expr2 expr2
  (* Behaviors. *)
  behavior1 behavior2
=
  (* Allow [TODO] to escape and abort the whole test. *)
  if behavior1 = Error TODO || behavior2 = Error TODO then
    raise TODO
  else if not (eq_behavior eq_value behavior1 behavior2) then
    fail (
      announcement @
      R.Text "This expression:" ::
      R.Break ::
      R.Code (show_expr1 expr1) ::
      R.Break ::
      R.Text "should be equivalent to the following expression:" ::
      R.Break ::
      R.Code (show_expr2 expr2) ::
      R.Break ::
      R.Text "Yet, the former expression" ::
      show_actual_behavior show_value behavior1 @
      R.Text "whereas the latter expression" ::
      show_actual_behavior show_value behavior2
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

(* an ad hoc variant of [test_value_3] *)
let test_value_3_dependent name ty reference printx1 printx2 printx3 showy eqy tests =
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
          (eqy x1 x2) showy
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

let show_option_move =
  wrap (print_option print_int)

(* -------------------------------------------------------------------------- *)

(* Generating game trees. *)

module G = struct

  open Feat

  (* We restrict the range of values to -1, 0, +1, which represent a loss,
     a draw, and a victory. *)

  let value : value enum =
    finite [ -1; 0; +1 ]

  let tree : tree enum =
    fix (fun tree ->
      map leaf value ++
      pay (
        map nonleaf (nonempty_list tree)
      )
    )

end

(* -------------------------------------------------------------------------- *)

(* Printing game trees. *)

module P = struct

  let undo (mts : (move * tree) Seq.t) : tree list =
    mts
    |> Seq.to_list
    |> List.map snd

  let rec tree t =
    match t with
    | TLeaf v ->
        apply "leaf" [ print_atomic_int v ]
    | TNonLeaf mts ->
        apply "nonleaf" [ list (map tree (undo mts)) ]

  let parens_tree t =
    parens (tree t)

  let show_sense = function Even -> "Even" | Odd -> "Odd"

  let sense sense = utf8string (show_sense sense)

end

(* -------------------------------------------------------------------------- *)

(* Grading [size] and [height]. *)

let trees : tree list =
  Feat.(sample 50 10 G.tree)

let test_size () =
  section "Question 1" (
    test_value_1
      "size" [%ty : tree -> int]
      Solution.size
      P.parens_tree
      show_int (=)
      trees
  )

let test_height () =
  section "Question 2" (
    test_value_1
      "height" [%ty : tree -> int]
      Solution.height
      P.parens_tree
      show_int (=)
      trees
  )

(* -------------------------------------------------------------------------- *)

(* Grading [eval]. *)

(* First, we compare [eval] against our reference implementation. *)

let pair x y =
  (x, y)

let sense_trees =
  map (pair Even) trees @
  map (pair Odd) trees

let test_eval_against_solution () =
  test_value_2
    "eval" [%ty: sense -> tree -> value]
    Solution.eval
    P.sense
    P.parens_tree
    show_int (=)
    sense_trees

(* Besides, we also test whether [eval] satisfies its specification.
   This can help point out mistakes in a different manner. The equation
   that we wish to test is:

     eval (opposite sense) t = - eval sense t

 *)

let test_eval_spec () =
  grab [%ty: sense -> tree -> value]  "eval" (fun eval ->
    protect (fun () ->
      sense_trees |> List.iter (fun (sense, t) ->
        let left = T.result (fun () -> eval (opposite sense) t)
        and right = T.result (fun () -> - eval sense t) in
        let context doc =
          def "sense" (P.sense sense) (
          def "t" (P.tree t) (
          doc))
        in
        let print_left () =
          context (utf8string "eval (opposite sense) t")
        and print_right () =
          context (utf8string "- (eval sense t)")
        in
        let message = [ R.Code "eval"; R.Text "is not symmetric."; ] in
        expect_equal_behavior
          (=) show_int
          message
          (wrap print_left) ()
          (wrap print_right) ()
          left right
      );
      let message = [ R.Code "eval"; R.Text "appears to be symmetric."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  )

(* Combine the two. *)

let test_eval () =
  section "Question 3" (
    test_eval_against_solution() @
    test_eval_spec()
  )

(* -------------------------------------------------------------------------- *)

(* Grading [nval]. *)

(* First, we compare [nval] against our reference implementation. *)

let test_nval_against_solution () =
  test_value_1
    "nval" [%ty: tree -> value]
    Solution.nval
    P.parens_tree
    show_int (=)
    trees

(* Besides, we also test whether [nval] satisfies its specification.
   This can help point out mistakes in a different manner. The equation
   that we wish to test is:

     interpret sense (nval t) = eval sense t

 *)

let test_nval_spec () =
  grab [%ty: sense -> tree -> value]  "eval" (fun eval ->
  grab [%ty: tree -> value]  "nval" (fun nval ->
    protect (fun () ->
      sense_trees |> List.iter (fun (sense, t) ->
        let left = T.result (fun () -> interpret sense (nval t))
        and right = T.result (fun () -> eval sense t) in
        let context doc =
          def "sense" (P.sense sense) (
          def "t" (P.tree t) (
          doc))
        in
        let print_left () =
          context (utf8string "interpret sense (nval t)")
        and print_right () =
          context (utf8string "eval sense t")
        in
        let message = [ R.Code "nval"; R.Text "does not respect its specification."; ] in
        expect_equal_behavior
          (=) show_int
          message
          (wrap print_left) ()
          (wrap print_right) ()
          left right
      );
      let message = [ R.Code "nval"; R.Text "appears to respect its specification."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  ))

(* Combine the two. *)

let test_nval () =
  section "Question 4" (
    test_nval_against_solution() @
    test_nval_spec()
  )

(* -------------------------------------------------------------------------- *)

(* Grading [ntval]. *)

(* We compare [ntval] against our reference implementation. *)

let test_ntval () =
  section "Question 5" (
    test_value_1
      "ntval" [%ty: tree -> value]
      Solution.ntval
      P.parens_tree
      show_int (=)
      trees
  )

(* -------------------------------------------------------------------------- *)

(* Grading [bval]. *)

(* There are several ways in which [bval] can be tested for functional
   correctness:

   - By comparing [bval bottom top] with [ntval] -- they should be equal.
     This is a special case of the next item:

   - by testing that [bval] satisfies its specification, i.e.,
     [bval alpha beta t] is equivalent to [ntval t],
     according a notion of equivalence that equates all values less than
     or equal to [alpha] and that equates all values greater than or equal
     to [beta].

   - by comparing [bval alpha beta] against our reference implementation,
     for arbitrary values of [alpha] and [beta]. To do so, it seems that
     we should use the same notion of equivalence as in the previous item.
     Otherwise, we would distinguish between minor variants of the algorithm,
     all of which are correct (they evaluate the same number of tree nodes,
     and return the same final result when [alpha, beta] is the full window
     [bottom, top]).

   We use only the first and third items; it would be a bit noisy to
   also run the second test (and display a message when it fails). *)

let alpha_beta_pairs = [
  -1, 0;
  -1, 1;
  0, 1;
]

let alpha_beta_trees : (value * value * tree) list =
  alpha_beta_pairs |> List.map (fun (alpha, beta) ->
    trees |> List.map (fun t ->
      alpha, beta, t
    )
  )
  |> List.flatten

let test_bval_against_solution () =
  test_value_3_dependent
    "bval" [%ty: value -> value -> tree -> value]
    Solution.bval
    print_atomic_int print_atomic_int
    P.parens_tree
    show_int equivalent
    alpha_beta_trees

let test_bval_spec () =
  grab [%ty: value -> value -> tree -> value]  "bval" (fun bval ->
  grab [%ty: tree -> value]  "ntval" (fun ntval ->
    protect (fun () ->
      trees |> List.iter (fun t ->
        let left = T.result (fun () -> bval bottom top t)
        and right = T.result (fun () -> ntval t) in
        let context doc = def "t" (P.tree t) doc in
        let print_left () =
          context (utf8string "bval bottom top t")
        and print_right () =
          context (utf8string "ntval t")
        in
        let message = [ R.Code "bval bottom top"; R.Text "does not respect its specification."; ] in
        expect_equal_behavior
          (=) show_int
          message
          (wrap print_left) ()
          (wrap print_right) ()
          left right
      );
      let message = [ R.Code "bval bottom top"; R.Text "appears to respect its specification."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  ))

(* Another way of testing [bval] is to count how many tree nodes are forced,
   and to complain if too many nodes are forced. (Or too few, but that seems
   unlikely, as the student's algorithm would then have to be incorrect.) *)

let rec instrument_offspring c offspring : offspring =
  fun () ->
    match offspring() with
    | Seq.Nil ->
        Seq.Nil
    | Seq.Cons ((move, t), offspring) ->
        incr c;
        Seq.Cons ((move, instrument_tree c t), instrument_offspring c offspring)

and instrument_tree c t : tree =
  match t with
  | TLeaf v ->
      t
  | TNonLeaf offspring ->
      TNonLeaf (instrument_offspring c offspring)

let check name actual_count expected_count print_test =
  if actual_count <> expected_count then
    let message =
      sprintf "appears to examine %s tree nodes than required."
        (if actual_count > expected_count then "more" else "fewer")
    in
    let reason =
      sprintf "evaluates %d tree nodes, whereas evaluating %d nodes is %s."
        (actual_count + 1)   (* add 1 to account for the root *)
        (expected_count + 1) (* add 1 to account for the root *)
        (if actual_count > expected_count then "sufficient" else "necessary")
    in
    fail (
      R.Code name ::
      R.Text message ::
      R.Text "The following expression:" ::
      R.Break ::
      R.Code (wrap print_test ()) ::
      R.Break ::
      R.Text reason ::
      []
    )

let test_bval_lazy () =
  grab [%ty: value -> value -> tree -> value]  "bval" (fun bval ->
    protect (fun () ->
      alpha_beta_trees |> List.iter (fun (alpha, beta, t) ->
        let test bval =
          let c = ref 0 in
          let t = instrument_tree c t in
          let _v : value = bval alpha beta t in
          !c
        in
        let print_test () =
          def "t" (P.tree t) (
          def "alpha" (print_int alpha) (
          def "beta" (print_int beta) (
          utf8string "bval alpha beta t"
          )))
        in
        let actual_behavior = T.result (fun () -> test bval) in
        let Ok actual_count = actual_behavior in
        let expected_count = test Solution.bval in
        check "bval" actual_count expected_count print_test
      );
      let message = [ R.Code "bval"; R.Text "appears to examine as few tree nodes as possible."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  )

(* Combine the above tests. *)

let test_bval() =
  section "Question 6" (
    test_bval_spec() @
    test_bval_against_solution() -@>
    test_bval_lazy
  )

(* -------------------------------------------------------------------------- *)

(* Grading [assured_win]. *)

let test_assured_win_against_solution () =
  test_value_1
    "assured_win" [%ty: tree -> bool]
    Solution.assured_win
    P.parens_tree
    show_bool (=)
    trees

let test_assured_win_lazy () =
  grab [%ty: tree -> bool]  "assured_win" (fun assured_win ->
    protect (fun () ->
      trees |> List.iter (fun t ->
        let test assured_win =
          let c = ref 0 in
          let t = instrument_tree c t in
          let _b : bool = assured_win t in
          !c
        in
        let print_test () =
          def "t" (P.tree t) (utf8string "assured_win t")
        in
        let actual_behavior = T.result (fun () -> test assured_win) in
        let Ok actual_count = actual_behavior in
        let expected_count = test Solution.assured_win in
        check "assured_win" actual_count expected_count print_test
      );
      let message = [ R.Code "assured_win"; R.Text "appears to examine as few tree nodes as possible."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  )

let test_assured_win () =
  section "Question 7" (
    test_assured_win_against_solution() -@>
    test_assured_win_lazy
  )

(* -------------------------------------------------------------------------- *)

(* Grading [bmove]. *)

let test_bmove_against_solution () =
  test_value_3_dependent
    "bmove" [%ty: value -> value -> tree -> move option]
    Solution.bmove
    print_atomic_int print_atomic_int
    P.parens_tree
    show_option_move (fun _alpha _beta -> (=))
    alpha_beta_trees

let test_bmove_lazy () =
  grab [%ty: value -> value -> tree -> move option]  "bmove" (fun bmove ->
    protect (fun () ->
      alpha_beta_trees |> List.iter (fun (alpha, beta, t) ->
        let test bmove =
          let c = ref 0 in
          let t = instrument_tree c t in
          let _move : move option = bmove alpha beta t in
          !c
        in
        let print_test () =
          def "t" (P.tree t) (
          def "alpha" (print_int alpha) (
          def "beta" (print_int beta) (
          utf8string "bmove alpha beta t"
          )))
        in
        let actual_behavior = T.result (fun () -> test bmove) in
        let Ok actual_count = actual_behavior in
        let expected_count = test Solution.bmove in
        check "bmove" actual_count expected_count print_test
      );
      let message = [ R.Code "bmove"; R.Text "appears to examine as few tree nodes as possible."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  )

let test_bmove () =
  section "Question 8" (
    test_bmove_against_solution() -@>
    test_bmove_lazy
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_size() @
  test_height() @
  test_eval() @
  test_nval() @
  test_ntval() @
  test_bval() @
  test_assured_win() @
  test_bmove() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
