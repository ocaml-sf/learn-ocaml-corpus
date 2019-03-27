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

let correct ?silent:(silent=false) name =
  if silent then
    []
  else
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

let test_value_1 ?silent:(silent=false) name ty reference printx showy eqy tests =
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
      correct ~silent name
    )
  )

let test_value_2 ?silent:(silent=false) name ty reference printx1 printx2 showy eqy tests =
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
      correct ~silent name
    )
  )

let test_value_3 ?silent:(silent=false) name ty reference printx1 printx2 printx3 showy eqy tests =
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
      correct ~silent name
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

let print_int32 i =
  utf8format "0x%X" i
    (* hexadecimal notation *)

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

(* A printer for sums. *)

let print_either print_left print_right = function
  | Inl x ->
      construct "Inl" [ print_left x ]
  | Inr x ->
      construct "Inr" [ print_right x ]

let print_atomic_either print_left print_right v =
  parens (print_either print_left print_right v)

let print_atomic_either_int_int =
  print_atomic_either print_int print_int

(* A printer for pairs. *)

let print_pair print_left print_right (x, y) =
  tuple [ print_left x; print_right y ]

let print_pair_int_int =
  print_pair print_int print_int

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

let print_list_bool =
  print_list print_bool

let show_list_bool =
  wrap print_list_bool

let print_list_int =
  print_list print_int

let show_list_int =
  wrap print_list_int

let print_list_bool_bool =
  print_list (print_pair print_bool print_bool)

let print_list_int_bool =
  print_list (print_pair print_int print_bool)

let print_list_int_int =
  print_list (print_pair print_int print_int)

let print_list_either_bool =
  print_list (print_pair (print_either print_int print_int) print_bool)

let print_list_int_int_bool =
  print_list (print_pair print_pair_int_int print_bool)

let print_list_list_int =
  print_list (print_list print_int)

let show_list_list_int =
  wrap print_list_list_int

let print_list_string =
  print_list print_string

let show_list_string =
  wrap print_list_string

let print_list_int32 =
  print_list print_int32

let show_list_int32 =
  wrap print_list_int32

let print_list_list_bool =
  print_list (print_list print_bool)

let show_list_list_bool =
  wrap print_list_list_bool

(* A printer for results. *)

let show_result = function
  | Lt -> "Lt"
  | Eq -> "Eq"
  | Gt -> "Gt"

(* A printer for the type [_ order]. *)

(* With a hack in the case of [OMap] to recognize [bool2int]. *)

let bool2int = (function false -> 0 | true -> 1)

let rec print_atomic_order : type k . k order -> document =
  fun o ->
    match o with
    | OTrue ->
        utf8string "OTrue"
    | _ ->
        parens (print_order o)

and print_order : type k . k order -> document =
  fun o ->
    match o with
    | OTrue ->
        print_atomic_order o
    | ONat bound ->
        construct "ONat" [ print_int bound ]
    | OSum (o1, o2) ->
        construct "OSum" [ print_order o1; print_order o2 ]
    | OProd (o1, o2) ->
        construct "OProd" [ print_order o1; print_order o2 ]
    | OMap (f, o) ->
        (* We cannot even test [f == bool2int], as this test is ill-typed! *)
        let f = "(function false -> 0 | true -> 1)" in
        construct "OMap" [ utf8string f; print_order o ]

(* -------------------------------------------------------------------------- *)

(* Some code is taken from the solution. *)

open Solution

(* -------------------------------------------------------------------------- *)

(* [pigeonhole_sort] *)

(* We test functional correctness, but not time complexity, as there is no
   easy way of doing so. *)

let bound = 4

let key : int enum =
  Feat.(finite (up 0 bound))

let tests : (int * (int * bool) list) list =
  (* Special cases: *)
  (0, []) ::
  (1, []) ::
  (1, [(0, true)]) ::
  (1, [(0, true); (0, false)]) ::
  Feat.(sample 50 8 (just bound ** list (key ** bool)))

let test_pigeonhole_sort () =
  section "Question 1" (
    test_value_2 "pigeonhole_sort" [%ty: int -> (int * bool) list -> bool list]
      pigeonhole_sort
      print_int
      print_list_int_bool
      show_list_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* [cmp] *)

(* [cmp] is painful. Its argument [o] has type [_ order], a GADT, whose values
   we cannot easily enumerate -- that would be a heterogeneous enumeration. So
   we manually test a few specific cases. *)

let test_cmp_OTrue () =
  let tests = [
    OTrue, 0, 1;
    OTrue, 1, 0;
    OTrue, 0, 0;
    OTrue, 1, 1;
  ] in
  test_value_3 ~silent:true "cmp" [%ty : int order -> int -> int -> result] cmp
    print_atomic_order
    print_int print_int
    show_result (=)
    tests

let test_cmp_ONat () =
  let tests = [
    ONat 1, 0, 0;
    ONat 2, 0, 1;
    ONat 2, 1, 0;
    ONat 2, 0, 0;
    ONat 2, 1, 1;
    ONat 256, 0, 255;
    ONat 256, 255, 0;
    ONat 256, 0, 0;
    ONat 256, 255, 255;
    ONat 256, 33, 147;
  ] in
  test_value_3 ~silent:true "cmp" [%ty : int order -> int -> int -> result] cmp
    print_atomic_order
    print_int print_int
    show_result (=)
    tests

let bit : int enum =
  Feat.(finite (up 0 2))

let either : (int, int) either enum =
  let inl x = Inl x
  and inr x = Inr x in
  Feat.(map inl bit ++ map inr bit)

let pair : (int * int) enum =
  Feat.(bit ** bit)

let test_cmp_OSum () =
  let o = OSum (ONat 2, ONat 2) in
  let f (x, y) = (o, x, y) in
  let tests = Feat.(sample 50 0 (map f (either ** either))) in
  test_value_3 ~silent:true "cmp" [%ty : (int, int) either order ->
             (int, int) either -> (int, int) either -> result] cmp
    print_atomic_order
    print_atomic_either_int_int print_atomic_either_int_int
    show_result (=)
    tests

let test_cmp_OProd () =
  let o = OProd (ONat 2, ONat 2) in
  let f (x, y) = (o, x, y) in
  let tests = Feat.(sample 50 0 (map f (pair ** pair))) in
  test_value_3 ~silent:true "cmp" [%ty : (int * int) order ->
                    (int * int) -> (int * int) -> result] cmp
    print_atomic_order
    print_pair_int_int print_pair_int_int
    show_result (=)
    tests

let test_cmp_OMap () =
  let o = OMap (bool2int, ONat 2) in
  let f (x, y) = (o, x, y) in
  let tests = Feat.(sample 50 0 (map f (bool ** bool))) in
  test_value_3 ~silent:true "cmp"
    [%ty : bool order -> bool -> bool -> result] cmp
    print_atomic_order
    print_bool print_bool
    show_result (=)
    tests

let test_cmp () =
  section "Question 2" (
    [] -@>
    test_cmp_OTrue -@>
    test_cmp_ONat -@>
    test_cmp_OSum -@>
    test_cmp_OProd -@>
    test_cmp_OMap -@>
    fun () -> correct "cmp"
  )

(* -------------------------------------------------------------------------- *)

(* [sort] *)

let test_sort_OTrue () =
  (* Same tests as [pigeonhole_sort] above. *)
  let tests = List.map (fun (bound, kvs) -> OTrue, kvs) tests in
  test_value_2 ~silent:true "sort"
    [%ty : int order -> (int * bool) list -> bool list] sort
    print_atomic_order
    print_list_int_bool
    show_list_bool (=)
    tests

let test_sort_ONat () =
  (* Same tests as [pigeonhole_sort] above. *)
  let tests = List.map (fun (bound, kvs) -> ONat bound, kvs) tests in
  test_value_2 ~silent:true "sort"
    [%ty : int order -> (int * bool) list -> bool list] sort
    print_atomic_order
    print_list_int_bool
    show_list_bool (=)
    tests

let test_sort_OSum () =
  let o = OSum (ONat 2, ONat 2) in
  let f kvs = (o, kvs) in
  let tests = Feat.(sample 50 4 (map f (list (either ** bool)))) in
  test_value_2 ~silent:true "sort"
    [%ty : (int, int) either order -> ((int, int) either * bool) list -> bool list]
    sort
    print_atomic_order
    print_list_either_bool
    show_list_bool (=)
    tests

let test_sort_OProd () =
  let o = OProd (ONat 2, ONat 2) in
  let f kvs = (o, kvs) in
  let tests = Feat.(sample 50 4 (map f (list ((bit ** bit) ** bool)))) in
  test_value_2 ~silent:true "sort"
    [%ty : (int * int) order -> ((int * int) * bool) list -> bool list]
    sort
    print_atomic_order
    print_list_int_int_bool
    show_list_bool (=)
    tests

let test_sort_OMap () =
  let o = OMap (bool2int, ONat 2) in
  let f kvs = (o, kvs) in
  let tests = Feat.(sample 50 4 (map f (list (bool ** bool)))) in
  test_value_2 ~silent:true "sort"
    [%ty : bool order -> (bool * bool) list -> bool list] sort
    print_atomic_order
    print_list_bool_bool
    show_list_bool (=)
    tests

let test_sort () =
  section "Question 3" (
    [] -@>
    test_sort_OTrue -@>
    test_sort_ONat -@>
    test_sort_OSum -@>
    test_sort_OProd -@>
    test_sort_OMap -@>
    fun () -> correct "sort"
  )

(* -------------------------------------------------------------------------- *)

(* [simple_sort] *)

(* We perform basic tests. Chances are, if [simple_sort] is type-correct, then
   it is correct, unless the student is actively trying to cheat. *)

let test_simple_sort () =
  section "Question 4" (
    let o = ONat bound in
    let tests = Feat.(sample 50 8 (just o ** list key)) in
    test_value_2 "simple_sort" [%ty: int order -> int list -> int list]
      simple_sort
      print_atomic_order
      print_list_int
      show_list_int (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* [bool] *)

(* [bool] is a constant, a user-defined order. We test it indirectly by
   testing [simple_sort bool]. Thus, we actually test [simple_sort] at
   the same time; in principle, if the student has passed the previous
   question, it should be correct, but it could be the case that a bug
   in [simple_sort] is detected here. *)

let test_bool () =
  section "Question 5" (
    grab [%ty: bool order -> bool list -> bool list] "simple_sort" (fun simple_sort ->
    grab [%ty: bool order] "bool" (fun bool ->
    let tests = Feat.(sample 50 4 (list bool)) in
    protect (fun () ->
      tests |> List.iter (fun vs ->
        let actual_behavior = T.result (fun () -> simple_sort bool vs)
        and expected_behavior = T.result (fun () -> Solution.(simple_sort bool vs)) in
        let print_expr () =
          apply "simple_sort" [ utf8string "bool"; print_list_bool vs ]
        in
        black_box_compare
          (=) show_list_bool
          (incorrect "bool")
          (wrap print_expr) ()
          actual_behavior
          expected_behavior
      );
      correct "bool"
    )))
  )

(* -------------------------------------------------------------------------- *)

(* [list] *)

(* [list] is a constant, a user-defined order. We test it indirectly by
   testing [simple_sort (list key)]. *)

let test_list () =
  section "Question 6" (
    grab [%ty: int list order -> int list list -> int list list] "simple_sort" (fun simple_sort ->
    grab [%ty: int order -> int list order] "list" (fun list ->
    let tests = Feat.(sample 50 8 (list (list key))) in
    protect (fun () ->
      tests |> List.iter (fun vs ->
        let key = ONat bound in
        let actual_behavior = T.result (fun () -> simple_sort (list key) vs)
        and expected_behavior = T.result (fun () -> Solution.(simple_sort (list key) vs)) in
        let print_expr () =
          apply "simple_sort" [ utf8format "(list (ONat %d))" bound; print_list_list_int vs ]
        in
        black_box_compare
          (=) show_list_list_int
          (incorrect "list")
          (wrap print_expr) ()
          actual_behavior
          expected_behavior
      );
      correct "list"
    )))
  )

(* -------------------------------------------------------------------------- *)

(* [string] *)

(* [string] is a constant, a user-defined order. We test it indirectly by
   testing [simple_sort string]. *)

let init (cs : char list) : string =
  let cs = Array.of_list cs in
  String.init (Array.length cs) (Array.get cs)

let test_string () =
  section "Question 7" (
    grab [%ty: string order -> string list -> string list] "simple_sort" (fun simple_sort ->
    grab [%ty: string order] "string" (fun string ->
    let char = Feat.finite ['%'; 'a'; 'A'; 'Z'] in
    let tests = Feat.(sample 50 8 (list (map init (list char)))) in
    protect (fun () ->
      tests |> List.iter (fun vs ->
        let actual_behavior = T.result (fun () -> simple_sort string vs)
        and expected_behavior = T.result (fun () -> Solution.(simple_sort string vs)) in
        let print_expr () =
          apply "simple_sort" [ utf8string "string"; print_list_string vs ]
        in
        black_box_compare
          (=) show_list_string
          (incorrect "string")
          (wrap print_expr) ()
          actual_behavior
          expected_behavior
      );
      correct "string"
    )))
  )

(* -------------------------------------------------------------------------- *)

(* [int32] *)

(* [int32] is a constant, a user-defined order. We test it indirectly by
   testing [simple_sort int32]. We also inspect it to check that it does
   not use [ONat bound] for a value of [bound] larger than 8. *)

let rec inspect : type k . k order -> unit =
  fun o -> match o with
  | OTrue ->
      ()
  | ONat bound ->
      if bound < 256 then
        fail [
          R.Text "Although"; R.Code "int32";
          R.Text "seems correct, its implementation involves";
          R.Code (sprintf "ONat %d" bound);
          R.Text "which seems unnecessary, as the use of";
          R.Code "ONat 256"; R.Text "is permitted.";
        ]
      else if bound > 256 then
        fail [
          R.Text "Although"; R.Code "int32";
          R.Text "seems correct, its implementation involves";
          R.Code (sprintf "ONat %d" bound);
          R.Text "which is not permitted.";
        ]
  | OSum (o1, o2) ->
      inspect o1;
      inspect o2
  | OProd (o1, o2) ->
      inspect o1;
      inspect o2
  | OMap (_, o) ->
      inspect o

let test_int32 () =
  section "Question 8" (
    grab [%ty: int order -> int list -> int list] "simple_sort" (fun simple_sort ->
    grab [%ty: int order] "int32" (fun int32 ->
    let data = Feat.finite [ 0; 1; 0x101; 0x10001; 0x230340; 0xDEADBEEF ] in
    let tests = Feat.(sample 50 4 (list data)) in
    protect (fun () ->
      tests |> List.iter (fun vs ->
        let actual_behavior = T.result (fun () -> simple_sort int32 vs)
        and expected_behavior = T.result (fun () -> Solution.(simple_sort int32 vs)) in
        let print_expr () =
          apply "simple_sort" [ utf8string "int32"; print_list_int32 vs ]
        in
        black_box_compare
          (=) show_list_int32
          (incorrect "int32")
          (wrap print_expr) ()
          actual_behavior
          expected_behavior
      );
      inspect int32;
      correct "int32"
    )))
  )

(* -------------------------------------------------------------------------- *)

(* [discr] *)

let test_discr_OTrue () =
  (* Same tests as [pigeonhole_sort] above. *)
  let tests = List.map (fun (bound, kvs) -> OTrue, kvs) tests in
  test_value_2 ~silent:true "discr"
    [%ty : int order -> (int * bool) list -> bool list list] discr
    print_atomic_order
    print_list_int_bool
    show_list_list_bool (=)
    tests

let test_discr_ONat () =
  (* Same tests as [pigeonhole_sort] above. *)
  let tests = List.map (fun (bound, kvs) -> ONat bound, kvs) tests in
  test_value_2 ~silent:true "discr"
    [%ty : int order -> (int * bool) list -> bool list list] discr
    print_atomic_order
    print_list_int_bool
    show_list_list_bool (=)
    tests

let test_discr_OSum () =
  let o = OSum (ONat 2, ONat 2) in
  let f kvs = (o, kvs) in
  let tests = Feat.(sample 50 4 (map f (list (either ** bool)))) in
  test_value_2 ~silent:true "discr"
    [%ty : (int, int) either order -> ((int, int) either * bool) list -> bool list list]
    discr
    print_atomic_order
    print_list_either_bool
    show_list_list_bool (=)
    tests

let test_discr_OProd () =
  let o = OProd (ONat 2, ONat 2) in
  let f kvs = (o, kvs) in
  let tests = Feat.(sample 50 4 (map f (list ((bit ** bit) ** bool)))) in
  test_value_2 ~silent:true "discr"
    [%ty : (int * int) order -> ((int * int) * bool) list -> bool list list]
    discr
    print_atomic_order
    print_list_int_int_bool
    show_list_list_bool (=)
    tests

let test_discr_OMap () =
  let o = OMap (bool2int, ONat 2) in
  let f kvs = (o, kvs) in
  let tests = Feat.(sample 50 4 (map f (list (bool ** bool)))) in
  test_value_2 ~silent:true "discr"
    [%ty : bool order -> (bool * bool) list -> bool list list] discr
    print_atomic_order
    print_list_bool_bool
    show_list_list_bool (=)
    tests

let test_discr () =
  section "Question 9" (
    [] -@>
    test_discr_OTrue -@>
    test_discr_ONat -@>
    test_discr_OSum -@>
    test_discr_OProd -@>
    test_discr_OMap -@>
    fun () -> correct "discr"
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =
  (* For determinism. *)
  Random.init 0

let report () =
  test_pigeonhole_sort() @
  test_cmp() @
  test_sort() @
  test_simple_sort() @
  test_bool() @
  test_list() @
  test_string() @
  test_int32() @
  test_discr() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
