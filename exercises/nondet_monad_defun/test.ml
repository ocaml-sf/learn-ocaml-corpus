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

let print_int_int (x, t) =
  parens (
    utf8format "(* value: *) %d, (* work: *) %d" x t
  )

let print_list_int_int =
  print_list print_int_int

let show_list_int_int =
  wrap print_list_int_int

(* -------------------------------------------------------------------------- *)

(* A DSL for monadic computations. *)

(* The syntax of pure expressions allows zero-ary operators (constants) of
   type [int] and unary operators of type ['a -> bool] and binary operators of
   type ['a -> 'a -> 'a]. Each operator is accompanied with its textual
   appearance as a plain OCaml expression. *)

(* In the type [('g, 'a) expr], ['g] is the type of every variable in scope,
   and ['a] is the type of the expression. *)

(* In the syntax of computations, we restrict the type of [CBind] so that
   every intermediate computation has result type ['a] and (therefore) every
   variable in scope has type ['a] as well. This removes the need for dealing
   with heterogeneous environments (both at the type level and value level),
   facilitates printing of computations, etc. *)

(* We ask the student to implement [at_most_once] rather than [once] because
   the former has a simpler type, which fits the restriction above. *)

type (_, _) expr =
  | EVar: int (* de Bruijn index *) -> ('g, 'g) expr
  | EConst: int -> ('g, int) expr

type fair =
  bool
    (* The fair variant of [choose] is [interleave]. *)
    (* The fair variant of [>>=] is [>>-]. *)

type 'a comp =
  (* Combinators implemented by me. *)
  | CDelay of 'a comp
  | CTick of 'a comp
  (* Core combinators. (Unfair variants.) (Question 1.) *)
  | CRet of ('a, 'a) expr
  | CBind of fair * 'a comp * 'a comp (* >>= *)
  | CFail
  | CChoose of fair * 'a comp * 'a comp
  (* More combinators. (Fair variants.) (Questions 2-4.) *)
  | CAtMostOnce of 'a comp

(* Generators. *)

module DSLGen = struct

  open Feat

  (* Integer expressions. *)

  (* [n] is the number of variables in scope. Memoization takes place over the
     pair [(n, s)], where [s] is the size parameter, which remains implicit. *)

  let evar x = EVar x
  let econst k = EConst k

  let int_expr : int -> (int, int) expr enum =
    fix2 (fun int_expr n ->
      map evar (finite (up 0 n)) ++
      map econst (finite [ 0; 1 ])
    )

  (* Commands. *)

  let cret e = CRet e
  let cbind (c1, c2) = CBind (false, c1, c2)
  let cdelay c = CDelay c
  let ctick c = CTick c
  let cchoose (c1, c2) = CChoose (false, c1, c2)
  let catmostonce c = CAtMostOnce c
  let cfairbind (c1, c2) = CBind (true, c1, c2)
  let cfairchoose (c1, c2) = CChoose (true, c1, c2)

  let core comp n =
    map cret (int_expr n) ++
    just CFail ++
    pay (
      map cbind (comp n ** comp (n+1)) ++
      map cdelay (comp n) ++
      map cchoose (comp n ** comp n)
    )

  (* Depending on the question number (Question 1, 2, etc.) we use more
     and more combinators. *)

  let q1 : int -> int comp enum =
    fix2 (fun comp n ->
      core comp n
    )

  let q2 : int -> int comp enum =
    fix2 (fun comp n ->
      core comp n ++
      pay (map catmostonce (comp n))
    )

  let q3 : int -> int comp enum =
    fix2 (fun comp n ->
      core comp n ++
      pay (map catmostonce (comp n)) ++
      pay (map cfairchoose (comp n ** comp n))
    )

  let q4 : int -> int comp enum =
    fix2 (fun comp n ->
      core comp n ++
      pay (map catmostonce (comp n)) ++
      pay (map cfairchoose (comp n ** comp n)) ++
      pay (map cfairbind (comp n ** comp n))
    )

end

(* Instrumenting a command with [tick] instructions. *)

(* We could insert [tick]s everywhere, but our counter-examples are a
   little more readable if we insert fewer ticks. *)

let rec instrument c =
  match c with
  | CRet _
  | CFail ->
      (* Tick [return] and [fail]. *)
      CTick c
  | CBind (fair, c1, c2) ->
      (* Do not tick [bind]. *)
      CBind (fair, instrument c1, instrument c2)
  | CDelay c ->
      (* Do not tick [delay]. *)
      CDelay (instrument c)
  | CTick c ->
      (* won't happen *)
      assert false
  | CChoose (fair, c1, c2) ->
      (* Do not tick [choose]. *)
      CChoose (fair, instrument c1, instrument c2)
  | CAtMostOnce c ->
      (* Do not tick [at_most_once]. *)
      CAtMostOnce (instrument c)

(* Printers. *)

module DSLPrinter = struct

  type senv =
    int

  let show_var senv x =
    assert (0 <= x && x < senv);
    (* Convert the de Bruijn index [x] to a de Bruijn level. *)
    let x = senv - 1 - x in
    (* Use a fixed conversion scheme. *)
    assert (x < 26);
    let c = Char.chr (Char.code 'a' + x) in
    String.make 1 c

  let print_var senv x =
    utf8string (show_var senv x)

  let rec print_atomic_expr : type g a . senv -> (g, a) expr -> document =
    fun senv e ->
      match e with
      | EVar x ->
          print_var senv x
      | EConst k ->
          print_int k
      | _ ->
          parens (print_expr senv e)

  and print_expr : type g a . senv -> (g, a) expr -> document =
    fun senv e ->
      match e with
      | EVar _ ->
          print_atomic_expr senv e
      | EConst _ ->
          print_atomic_expr senv e

  let rec print_atomic_comp senv c =
    match c with
    | CFail ->
        utf8string "fail"
    | _ ->
        parens (print_comp senv c)

  and print_tight_comp senv c =
    match c with
    | CFail ->
        print_atomic_comp senv c
    | CRet e ->
        apply "return" [ print_atomic_expr senv e ]
    | CDelay c ->
        apply "delay" [ print_atomic_comp senv c ]
    | CTick c ->
        apply "tick" [ print_atomic_comp senv c ]
    | CAtMostOnce c ->
        apply "at_most_once" [ print_atomic_comp senv c ]
    | CChoose (fair, c1, c2) ->
        apply
          (if fair then "interleave" else "choose")
          [ print_atomic_comp senv c1; print_atomic_comp senv c2 ]
    | _ ->
        parens (print_comp senv c)

  and print_comp senv c =
    group begin match c with
    | CBind (fair, c1, c2) ->
        let op = if fair then ">>-" else ">>=" in
        group (
          print_tight_comp senv c1 ^^ break 1 ^^
          utf8format "%s fun %s ->" op (show_var (senv+1) 0)
        ) ^^ break 1 ^^
        print_tight_comp (senv+1) c2
    | _ ->
        print_tight_comp senv c
    end

  let print_atomic_comp c =
    print_atomic_comp 0 c

  let to_list doc =
    piped_apply "Seq.to_list" [ doc ]

  let take depth doc =
    piped_apply "Seq.take" [ print_int depth; doc ]

  let sols doc =
    apply "sols" [ doc ]

  let snapshot doc =
    apply "Seq.map" [ utf8string "snapshot"; doc ]

  let print_comp_sols_take depth c =
    to_list (take depth (sols (print_atomic_comp c)))

  let show_comp_sols_take depth =
    wrap (print_comp_sols_take depth)

  let print_comp_sols_snapshot_take depth c =
    to_list (take depth (snapshot (sols (print_atomic_comp c))))

  let show_comp_sols_snapshot_take depth =
    wrap (print_comp_sols_snapshot_take depth)

end

(* A DSL interpreter. *)

(* An expression evaluator. *)

(* This evaluator is parameterized over an implementation of the signature
   [REQUIRED]. *)

(* Because (I think) we can only grab the student's code at a monomorphic
   type, we cannot require these functions to be polymorphic. We require
   just the monomorphic instances that we need for our test. *)

type mono =
  int

module type REQUIRED = sig
  val return: mono -> mono m
  val (>>=) : mono m -> (mono -> mono m) -> mono m
  val fail: mono m
  val choose: mono m -> mono m -> mono m
  (* The following components are optional, as they appear in
     Questions 2-4. *)
  val at_most_once: (mono m -> mono m) option
  val interleave: (mono m -> mono m -> mono m) option
  val (>>-): (mono m -> (mono -> mono m) -> mono m) option
end

module S = struct
  include Solution
  let at_most_once = Some at_most_once
  let interleave = Some interleave
  let (>>-) = Some (>>-)
end

let solution =
  (module S : REQUIRED)

let project o =
  match o with
  | Some x -> x
  | None   -> assert false

module DSLInterpreter = struct

  let rec eval_expr : type g a . g list -> (g, a) expr -> a =
    fun env e ->
      match e with
      | EVar x ->
          List.nth env x
      | EConst k ->
          k

  let eval_comp required (env : mono list) (c : mono comp) : mono m =
    let module R = (val required : REQUIRED) in
    let open R in
    let rec eval_comp env c =
      match c with
      | CRet e ->
          return (eval_expr env e)
      | CBind (fair, c1, c2) ->
          let bind = if fair then project (>>-) else (>>=) in
          bind
            (eval_comp env c1)
            (fun x1 -> eval_comp (x1 :: env) c2)
      | CDelay c ->
          delay (fun () -> eval_comp env c)
      | CTick c ->
          tick (eval_comp env c)
      | CFail ->
          fail
      | CChoose (fair, c1, c2) ->
          let plus = if fair then project interleave else choose in
          plus (eval_comp env c1) (eval_comp env c2)
      | CAtMostOnce c ->
          let at_most_once = project at_most_once in
          at_most_once (eval_comp env c)
    in
    eval_comp env c

  let eval_comp required (c : mono comp) : mono m =
    eval_comp required [] c

end

(* -------------------------------------------------------------------------- *)

(* Our black-box grading machinery. *)

(* In Question 1, we grade four construction functions: [return], [>>=],
   [fail], [choose], observed through one observation function, [sols].
   In Questions 2-4, we gradually add three more construction functions,
   [at_most_once], [interleave], and [>>-]. The testing machinery is the
   same. *)

module P = DSLPrinter
module I = DSLInterpreter

(* In order to avoid trouble with infinite sequences, we compare and print
   sequences only down to a certain [depth]. *)

let depth =
  20

(* First, we test functional correctness. We generate a computation using the
   constructor functions, and apply [sols] to it, yielding a sequence. We
   truncate this sequence at depth [depth] and force its evaluation by turning
   it into a list. Then, we check that this list of results is correct by
   comparing against our solution. *)

let test_correctness tests student sols =
  tests |> List.iter (fun (c : mono comp) ->
    let expected_behavior =
      Ok (
        I.eval_comp solution c
        |> Solution.sols
        |> Seq.take depth
        |> Seq.to_list
      )
    in
    let actual_behavior =
      T.result (fun () ->
        I.eval_comp student c
        |> sols
        |> Seq.take depth
        |> Seq.to_list
      )
    in
    black_box_compare
      (=) show_list_int
      something_is_wrong
      (P.show_comp_sols_take depth) c
      actual_behavior
      expected_behavior
  );
  (* Success. *)
  let points = 1 in
  let msg =
    sprintf "The code seems correct. Tested %d scenarios."
      (List.length tests)
  in
  [ R.success points msg ]

(* Second, we test that computations are performed on demand, that is, as late
   as possible. That is, demanding the next result should force execution to
   take place only as far as necessary in order to produce this result. To
   check this, we instrument our test computations with [tick] instructions,
   which increment the global counter [time], and decorate every result with
   the time at which it is produced. (A call to [Seq.map snapshot] suffices
   for this purpose.) We check that these times are correct by comparing
   against our solution. *)

let test_laziness tests student sols =
  let tests = List.map instrument tests in
  tests |> List.iter (fun (c : mono comp) ->
    let expected_behavior =
      Ok (
        reset();
        I.eval_comp solution c
        |> Solution.sols
        |> Seq.map snapshot
        |> Seq.take depth |> Seq.to_list
      )
    in
    let actual_behavior =
      T.result (fun () ->
        reset();
        I.eval_comp student c
        |> sols
        |> Seq.map snapshot
        |> Seq.take depth |> Seq.to_list
      )
    in
    black_box_compare
      (=) show_list_int_int
      [ R.Text "Some computations take place too early." ]
      (P.show_comp_sols_snapshot_take depth) c
      actual_behavior
      expected_behavior
  );
  (* Success. *)
  let points = 1 in
  let msg = "The code seems lazy." in
  [ R.success points msg ]

(* Combine the above two phases. *)

let test_both tests student sols =
  section "Functional correctness" (
    protect (fun () ->
      test_correctness tests student sols
    )
  ) -@> fun () ->
  section "Laziness" (
    protect (fun () ->
      test_laziness tests student sols
    )
  )

(* -------------------------------------------------------------------------- *)

(* Question 1. *)

(* Grading the core functions: [return], [>>=], [fail], [choose], [sols]. *)

let grab_core k =
  (* Grab the student's five basic functions. *)
  grab [%ty: mono -> mono m] "return" (fun return ->
  grab [%ty: mono m -> (mono -> mono m) -> mono m] ">>=" (fun (>>=) ->
  grab [%ty: mono m] "fail" (fun fail ->
  grab [%ty: mono m -> mono m -> mono m] "choose" (fun choose ->
  grab [%ty: mono m -> mono Seq.t] "sols" (fun sols ->
  let module S = struct
    let return = return
    let (>>=) = (>>=)
    let fail = fail
    let choose = choose
    let at_most_once = None
    let interleave = None
    let (>>-) = None
  end in
  let student = (module S : REQUIRED) in
  k student sols
  )))))

let test_core () =
  section "Question 1" (
    grab_core (fun student sols ->
      (* Generate test cases. *)
      let tests = Feat.sample 1000 4 (DSLGen.q1 0) in
      (* Test. *)
      test_both tests student sols
    )
  )

(* -------------------------------------------------------------------------- *)

(* Question 2. *)

(* Grading [at_most_once]. *)

let grab_at_most_once k =
  grab_core (fun student sols ->
  grab [%ty: mono m -> mono m] "at_most_once" (fun candidate ->
  let module S = struct
    include (val student : REQUIRED)
    let at_most_once = Some candidate
  end in
  let student = (module S : REQUIRED) in
  k student sols
  ))

let test_at_most_once () =
  section "Question 2" (
    grab_at_most_once (fun student sols ->
      (* Generate test cases. *)
      let tests = Feat.sample 1000 4 (DSLGen.q2 0) in
      (* Test. *)
      test_both tests student sols
    )
  )

(* -------------------------------------------------------------------------- *)

(* Question 3. *)

(* Grading [interleave]. *)

let grab_interleave k =
  grab_at_most_once (fun student sols ->
  grab [%ty: mono m -> mono m -> mono m] "interleave" (fun candidate ->
  let module S = struct
    include (val student : REQUIRED)
    let interleave = Some candidate
  end in
  let student = (module S : REQUIRED) in
  k student sols
  ))

let test_interleave () =
  section "Question 3" (
    grab_interleave (fun student sols ->
      (* Generate test cases. *)
      let tests = Feat.sample 1000 4 (DSLGen.q3 0) in
      (* Test. *)
      test_both tests student sols
    )
  )

(* -------------------------------------------------------------------------- *)

(* Question 4. *)

(* Grading [>>-]. *)

let grab_fair_bind k =
  grab_interleave (fun student sols ->
  grab [%ty: mono m -> (mono -> mono m) -> mono m] ">>-" (fun candidate ->
  let module S = struct
    include (val student : REQUIRED)
    let (>>-) = Some candidate
  end in
  let student = (module S : REQUIRED) in
  k student sols
  ))

let test_fair_bind () =
  section "Question 4" (
    grab_fair_bind (fun student sols ->
      (* Generate test cases. *)
      let tests = Feat.sample 3000 5 (DSLGen.q4 0) in
      (* Test. *)
      test_both tests student sols
    )
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_core() -@>
  test_at_most_once -@>
  test_interleave -@>
  test_fair_bind -@>
  fun () -> []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
