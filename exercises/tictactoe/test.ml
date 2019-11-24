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

let backslash =
  char '\\'

let doublequote =
  char '"'

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

type _state =
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

let tested inputs =
  let message = [ R.Text (sprintf "Tested %d inputs." (List.length inputs)); ] in
  [ R.Message (message, R.Success 0) ]

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

let test_value_4 name ty reference printx1 printx2 printx3 printx4 showy eqy tests =
  grab ty name (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2, x3, x4) as x) ->
        let actual_behavior = T.result (fun () -> candidate x1 x2 x3 x4)
        and expected_behavior = T.result (fun () -> reference x1 x2 x3 x4) in
        let print_expr () =
          apply name [ printx1 x1; printx2 x2; printx3 x3; printx4 x4 ]
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

let test_value_5 name ty reference printx1 printx2 printx3 printx4 printx5 showy eqy tests =
  grab ty name (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2, x3, x4, x5) as x) ->
        let actual_behavior = T.result (fun () -> candidate x1 x2 x3 x4 x5)
        and expected_behavior = T.result (fun () -> reference x1 x2 x3 x4 x5) in
        let print_expr () =
          apply name [ printx1 x1; printx2 x2; printx3 x3; printx4 x4; printx5 x5 ]
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

(* Memoization utilities. *)

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

(* Miscellaneous printers. *)

let print_pair_int =
  print_pair print_int print_int

let show_pair_int =
  wrap print_pair_int

(* The Cartesian product of two lists, transformed by [f]. *)

let cartesian_product xs ys f accu =
  List.fold_right (fun x accu ->
    List.fold_right (fun y accu ->
      f x y :: accu
    ) ys accu
  ) xs accu

let cartesian_product_3 xs ys zs f accu =
  List.fold_right (fun x accu ->
    List.fold_right (fun y accu ->
      List.fold_right (fun z accu ->
        f x y z :: accu
      ) zs accu
    ) ys accu
  ) xs accu

(* A printer for bitmaps in two dimensions. The bitmap is printed on several
   lines (as dictated by [w] and [h]), with a backslash at the end of each
   line, so it can later become part of an OCaml string literal and passed as
   an argument to the function [bitmap] define in [Prelude]. *)

let print_bit (b : bool) =
  utf8string (if b then "1" else "0")

let print_bitmap w h bitmap =
  let line j : document =
    (
      up 0 (w+1)
      |> List.map (fun i -> print_bit (Solution.read w h bitmap i j))
      |> concat
    )
    ^^ hardline
  in
  up 0 (h+1)
  |> List.rev
  |> List.map line
  |> concat

let print_bitmap w h bitmap =
  utf8format "(bitmap %d %d " w h ^^ nest 2 (
    doublequote ^^ hardline ^^
    print_bitmap w h bitmap
  ) ^^ doublequote ^^ utf8string ")"

(* A horrible hack: use global mutable state to transmit [w] and [h]. *)

let global_w, global_h =
  ref 0, ref 0

let setting_wh f w h =
  global_w := w;
  global_h := h;
  f w h

let print_bitmap bitmap =
  let w = !global_w
  and h = !global_h in
  print_bitmap w h bitmap

let show_bitmap =
  wrap print_bitmap

(* -------------------------------------------------------------------------- *)

(* Grading [decode]. *)

let dimensions : (int * int) list = [
  3, 3;
  4, 4;
  4, 3;
  3, 4;
]

let offsets : int list =
  up 0 Sys.word_size

let test_decode () =
  section "Question 1" (
    test_value_3
      "decode" [%ty : int -> int -> offset -> int * int]
      Solution.decode
      print_int print_int print_int
      show_pair_int (=)
      (cartesian_product dimensions offsets (fun (w, h) o -> w, h, o) [])
  )

(* -------------------------------------------------------------------------- *)

(* Grading [mask]. *)

let dimensions_and_coordinates =
  List.fold_right (fun (w, h) accu ->
    cartesian_product (up 0 w) (up 0 h) (fun i j -> w, h, i, j) accu
  ) dimensions []

let test_mask () =
  section "Question 2" (
    test_value_4
      "mask" [%ty : int -> int -> int -> int -> bitmap]
      (setting_wh Solution.mask)
      print_int print_int print_int print_int
      show_bitmap (=)
      dimensions_and_coordinates
  )

(* -------------------------------------------------------------------------- *)

(* Sample bitmaps. *)

(* A mask for the leftmost column. *)
let columnmask w h =
  (1 lsl h) - 1

(* A mask for the leftmost column, including the top row. *)
let columnmask1 w h =
  (1 lsl (h + 1)) - 1

(* All bits set, including the top row, but not the rightmost column. *)
let all1 w h =
  let size1 = (h + 1) * w in
  (1 lsl size1) - 1

(* A mask for the bottom row (???). *)
let bottom w h =
  all1 w h / columnmask1 w h

(* A mask for the top row. *)
let top w h =
  bottom w h lsl h

(* All bits set, excluding the top row and rightmost column. *)
let all w h =
  all1 w h lxor top w h

(* Random bitmaps of size [w + 1] by [h + 1]. *)

let bitmaps : int * int -> bitmap list =
  memoize (fun (w, h) ->
    let w1 = w + 1
    and h1 = h + 1 in
    let size = w1 * h1 in
    (* The empty bitmap: *)
    let empty = 0 in
    (* The full bitmap: *)
    let full = (1 lsl size) - 1 in
    empty ::
    full ::
    (* Random bitmaps: *)
    SymSeq.(sample 50 (interval 1 full))
  )

(* Random bitmaps of size [w + 1] by [h + 1], with an empty top column
   and rightmost row. *)

let valid_bitmaps : int * int -> bitmap list =
  memoize (fun (w, h) ->
    let mask = all w h in
    List.map (fun bitmap ->
      bitmap land mask
    ) (bitmaps (w, h))
  )

(* -------------------------------------------------------------------------- *)

(* Grading [read]. *)

let inputs =
  List.fold_right (fun (w, h, i, j) accu ->
    List.map (fun bitmap -> w, h, bitmap, i, j) (bitmaps (w, h)) @
    accu
  ) dimensions_and_coordinates []

let test_read () =
  section "Question 3" (
    test_value_5
      "read" [%ty : int -> int -> bitmap -> int -> int -> bool]
      (setting_wh Solution.read)
      print_int print_int print_bitmap print_int print_int
      show_bool (=)
      inputs
  )

(* -------------------------------------------------------------------------- *)

(* Grading [update]. *)

let test_update () =
  section "Question 4" (
    test_value_5
      "update" [%ty : int -> int -> bitmap -> int -> int -> bitmap]
      (setting_wh Solution.update)
      print_int print_int print_bitmap print_int print_int
      show_bitmap (=)
      inputs
  )

(* -------------------------------------------------------------------------- *)

(* Grading the directions. *)

let directions =
  Solution.([
    "north", north;
    "east", east;
    "northeast", northeast;
    "southeast", southeast;
  ])

let print_direction direction =
  utf8string Solution.(
    if direction == north then "north"
    else if direction == east then "east"
    else if direction == northeast then "northeast"
    else if direction == southeast then "southeast"
    else "???"
  )

let test_directions () =
  section "Question 5" (
    List.fold_left (fun accu (name, reference) ->
      accu @
      test_value_2
        name [%ty : direction]
        reference
        print_int print_int
        show_int (=)
        dimensions
    ) [] (List.tl directions) (* all except [north] *)
  )

let directions =
  List.map snd directions

(* -------------------------------------------------------------------------- *)

(* Grading [alignments]. *)

let inputs : (int * int * int * direction * bitmap) list =
  List.fold_right (fun (w, h) accu ->
    cartesian_product_3
      (up 1 5)
      directions
      (valid_bitmaps (w, h))
      (fun k direction bitmap -> w, h, k, direction, bitmap)
      accu
  ) dimensions []

let test_alignments () =
  section "Question 6" (
    test_value_5
      "alignments" [%ty : int -> int -> int -> direction -> bitmap -> bitmap]
      (setting_wh Solution.alignments)
      print_int print_int print_int print_direction print_bitmap
      show_bitmap (=)
      inputs
  )

(* -------------------------------------------------------------------------- *)

(* Grading [has_alignment], using the same inputs as above. *)

let inputs : (int * int * int * bitmap) list =
  List.fold_right (fun (w, h) accu ->
    cartesian_product
      (up 1 4)
      (valid_bitmaps (w, h))
      (fun k bitmap -> w, h, k, bitmap)
      accu
  ) dimensions []

let test_has_alignment () =
  section "Question 7" (
    test_value_4
      "has_alignment" [%ty : int -> int -> int -> bitmap -> bool]
      (setting_wh Solution.has_alignment)
      print_int print_int print_int print_bitmap
      show_bool (=)
      inputs
  )

(* -------------------------------------------------------------------------- *)

(* Computing the length of a sequence, with a bound. *)

let rec bseqlength n xs =
  match xs() with
  | Seq.Nil ->
      0
  | Seq.Cons (_, xs) ->
      if n > 0 then
        1 + bseqlength (n-1) xs
      else
        0

(* -------------------------------------------------------------------------- *)

(* Comparing two game trees, breadth first. *)

(* The fact that the reference tree is finite guarantees termination. *)

(* [compare reference candidate] normally succeeds silently. However,
   it can also raise [Fail (path, msg)] where [path] is a path into
   both trees and [msg] is an error message. It can also raise [Length
   (path, e)] where [path] is a path and [e] is a student exception.
   This means that attempting to count the children of the subtree at
   [path] has caused this exception. *)

type path = int list

exception Fail of path * string

exception Length of path * exn

module Q = Queue

let compare (reference : tree) (candidate : tree) : unit =
  let q = Q.create() in
  Q.add (reference, candidate, []) q;
  while not (Q.is_empty q) do
    let t0, t1, path = Q.take q in
    let fail msg = raise (Fail (List.rev path, msg)) in
    match t0, t1 with
    | TLeaf _, TNonLeaf _ ->
        fail "A leaf is expected, yet a node is found."
    | TNonLeaf _, TLeaf _ ->
        fail "A node is expected, yet a leaf is found."
    | TLeaf v0, TLeaf v1 ->
        if v0 <> v1 then
          fail (sprintf "A leaf of value %d is expected, \
                         yet a leaf of value %d is found." v0 v1)
    | TNonLeaf offspring0, TNonLeaf offspring1 ->
        let n0 = Seq.length offspring0 in
        match bseqlength (n0+1) offspring1 with
        (* Attempting to measure the length of the sequence [offspring1]
           is dangerous, as it gives control to student code. In particular,
           an exception can be raised, which we must detect. *)
        | exception e ->
            raise (Length (path, e))
        | n1 ->
            if n1 < n0 then
              fail (sprintf "A node with %d children is expected, \
                             yet a node with %d children is found." n0 n1)
            else if n0 < n1 then
              (* In this case, [n1] must be [n0+1], as we have aborted. *)
              fail (sprintf "A node with %d children is expected, \
                             yet a node with more children is found." n0);
            (* We now know that the sequences have the same length. *)
            (* Let's check that they have the same move indices. We sort
               them by increasing move index (in fact, [moves0] is already
               sorted in this way) and compare them. *)
            let offspring0 = Seq.to_list offspring0
            and offspring1 =
              Seq.to_list offspring1
              |> List.sort (fun (move1, _) (move2, _) -> compare move1 move2)
            in
            let moves0 = List.map fst offspring0
            and moves1 = List.map fst offspring1 in
            if moves0 <> moves1 then
              fail (sprintf "A node with children labeled %s is expected, \
                             yet a node with children labeled %s is found."
                      (show_list_int moves0)
                      (show_list_int moves1)
                   );
            (* We know now that the moves are the same. Enqueue the subtrees
               for later comparison. *)
            List.iter2 (fun (move0, t0) (move1, t1) ->
              assert (move0 = move1);
              Q.add (t0, t1, move0 :: path) q
            ) offspring0 offspring1
  done

(* -------------------------------------------------------------------------- *)

(* Grading [tree]. *)

(* This is ambitious: we test the output of the final function, [tree],
   without testing the auxiliary functions that operate on game states
   and must be written by the student before writing [tree]. *)

(* We rely on the assumption that the student code is purely functional.
   Indeed, we run the student code via [compare] -- which enumerates all
   paths -- but once we have found a problem at a specific path, we
   display an error message that concerns this path only. (We do this
   by showing an expression of the form [descendant path t].) *)

let tests : (int * int * int) list = [
  (* w, h, k *)
  1, 1, 1;
  1, 1, 2; (* always a draw! *)
  2, 2, 1;
  3, 3, 1;
  2, 2, 2;
  3, 2, 2;
  2, 3, 2;
  3, 3, 2;
  2, 2, 3; (* always a draw! *)
  (* 3, 3, 3, is too expensive! *)
]

let print_defs path w h k body =
  def "w" (print_int w) (
  def "h" (print_int h) (
  def "k" (print_int k) (
  def "t" (utf8string "tree (initial w h k)") (
    let print_move move =
      (* We could print [move] directly as an integer, but we prefer
         to print it as an application of [encode] to two integers [i]
         and [j]; this is more readable. (We hope that the student's
         [encode] function is correct.) *)
      let i, j = Solution.decode w h move in
      apply "encode" [ utf8string "w"; utf8string "h"; print_int i; print_int j ]
    in
    def "path" (print_list print_move path) (
      body
    )
  ))))

let print_expr path w h k () =
  let body = utf8string "descendant path t" in
  print_defs path w h k body

let show_expr path w h k =
  wrap (print_expr path w h k) ()

let print_len_expr path w h k () =
  let body = utf8string "descendant path t" in
  (* We could show a call to [Seq.length], but a call to [Seq.to_list]
     shows in a more explicit manner that the sequence elements are
     demanded by this call. *)
  let body = apply "Seq.to_list" [ parens body ] in
  print_defs path w h k body

let show_len_expr path w h k =
  wrap (print_len_expr path w h k) ()

let test_tree () =
  section "Question 8" (
    grab [%ty : state -> tree] "tree" (fun tree ->
    grab [%ty : int -> int -> int -> state] "initial" (fun initial ->
    protect (fun () ->
      tests |> List.iter (fun (w, h, k) ->
        let actual_behavior = T.result (fun () -> tree (initial w h k))
        and expected_tree = Solution.(tree (initial w h k)) in
        match actual_behavior with
        | Error TODO ->
            raise TODO
        | Error e ->
            fail (
              something_is_wrong @
              R.Text "The following expression:" ::
              R.Break ::
              R.Code (show_expr [] w h k) ::
              R.Break ::
              R.Text "raises the following exception:" ::
              R.Output (Printexc.to_string e) ::
              []
            )
        | Ok actual_tree ->
            try
              compare expected_tree actual_tree
            with
            | Fail (path, msg) ->
                fail (
                  something_is_wrong @
                  R.Text "The following expression produces an incorrect subtree:" ::
                  R.Break ::
                  R.Code (show_expr path w h k) ::
                  R.Break ::
                  R.Text msg ::
                  []
                )
            | Length (path, e) ->
                fail (
                  something_is_wrong @
                  R.Text "The following expression:" ::
                  R.Break ::
                  R.Code (show_len_expr path w h k) ::
                  R.Break ::
                  R.Text "raises the following exception:" ::
                  R.Output (Printexc.to_string e) ::
                  []
                )
      );
      (* Success. *)
      let message = [ R.Code "tree"; R.Text "and"; R.Code "initial";
                      R.Text "seem correct."; ] in
      [ R.Message (message, R.Success 1) ]
    )))
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_decode() @
  test_mask() @
  test_read() @
  test_update() @
  test_directions() @
  test_alignments() @
  test_has_alignment() @
  test_tree() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
