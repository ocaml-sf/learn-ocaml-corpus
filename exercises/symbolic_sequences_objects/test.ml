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

let apply f docs =
  group (nest 2 (concat (separate (break 1) (utf8string f :: docs))))

let wrap (print : 'a -> document) : 'a -> string =
  fun x -> pretty 70 (group (print x))

(* -------------------------------------------------------------------------- *)

(* FixMini. *)

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

(* FeatMini. *)

module Seq = struct

  type _ seq =
  | Empty    : 'a seq
  | Singleton: 'a -> 'a seq
  | Sum      : int * 'a seq * 'a seq -> 'a seq
  | Product  : int * 'a seq * 'b seq -> ('a * 'b) seq
  | Map      : int * ('a -> 'b) * 'a seq -> 'b seq
  | Up       : int * int -> int seq

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
    | Up (a, b) ->
        b - a

  let is_empty s =
    length s = 0

  let out_of_bounds () =
    failwith "Index is out of bounds."

  let empty =
    Empty

  let singleton x =
    Singleton x

  let sum s1 s2 =
    if is_empty s1 then
      s2
    else if is_empty s2 then
      s1
    else
      let length = length s1 + length s2 in
      Sum (length, s1, s2)

  let product s1 s2 =
    if is_empty s1 || is_empty s2 then
      empty
    else
      let length = length s1 * length s2 in
      Product (length, s1, s2)

  let map phi s =
    if is_empty s then
      empty
    else
      Map (length s, phi, s)

  let up a b =
    if a < b then
      Up (a, b)
    else
      Empty

  let rec get : type a . a seq -> int -> a =
    fun s i ->
      match s with
      | Empty ->
          out_of_bounds()
      | Singleton x ->
          if i = 0 then x else out_of_bounds()
      | Sum (_, s1, s2) ->
          let n1 = length s1 in
          if i < n1 then get s1 i
          else get s2 (i - n1)
      | Product (_, s1, s2) ->
          let q, r = i / length s2, i mod length s2 in
          get s1 q, get s2 r
      | Map (_, phi, s) ->
          phi (get s i)
      | Up (a, b) ->
          let x = a + i in
          if x < a || b <= x then
            out_of_bounds()
          else
            x

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
      | Up (a, b) ->
          for i = a to b - 1 do
            k i
          done

  let bigsum ss =
    List.fold_left sum empty ss

end

module Enum = struct

  type 'a enum =
    int -> 'a Seq.seq

  let empty : 'a enum =
    fun _s ->
      Seq.empty

  let zero =
    empty

  let enum (xs : 'a Seq.seq) : 'a enum =
    fun s ->
      if s = 0 then xs else Seq.empty

  let just (x : 'a) : 'a enum =
    fun s ->
      if s = 0 then Seq.singleton x else Seq.empty

  let pay (enum : 'a enum) : 'a enum =
    fun s ->
      if s = 0 then Seq.empty else enum (s-1)

  let sum (enum1 : 'a enum) (enum2 : 'a enum) : 'a enum =
    fun s ->
      Seq.sum (enum1 s) (enum2 s)

  let ( ++ ) =
    sum

  let rec up i j =
    if i <= j then
      i :: up (i + 1) j
    else
      []

  let product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
    fun s ->
      Seq.bigsum (
        List.map (fun s1 ->
          let s2 = s - s1 in
          Seq.product (enum1 s1) (enum2 s2)
        ) (up 0 s)
      )

  let ( ** ) =
    product

  let map (phi : 'a -> 'b) (enum : 'a enum) : 'b enum =
    fun s ->
      Seq.map phi (enum s)

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
  T.test_value (T.lookup_student ty name) (fun candidate ->
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

let mismatch name x actual expected n displayx showy =
  fail (
    R.Code name :: R.Text "is incorrect." ::
    R.Break ::
    let plural = if n > 1 then "s" else "" in
    R.Text (sprintf "When applied to the following argument%s:" plural) ::
    R.Break ::
    displayx x @
    R.Text "it produces the following invalid result:" ::
    R.Break ::
    R.Output (showy actual) ::
    R.Text "A valid result is:" ::
    R.Break ::
    R.Output (showy expected) ::
    []
  )

let codebreak show x =
  [ R.Code (show x); R.Break ]

let test_value_1 name ty reference showx showy eqy tests =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun x ->
        let actual = candidate x
        and expected = reference x in
        if not (eqy actual expected) then
          mismatch name x actual expected
            1 (codebreak showx)
            showy
      );
      correct name
    )
  )

let test_value_2 name ty reference showx1 showx2 showy eqy tests =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2) as x) ->
        let actual = candidate x1 x2
        and expected = reference x1 x2 in
        if not (eqy actual expected) then
          mismatch name x actual expected
            2 (fun (x1, x2) -> codebreak showx1 x1 @ codebreak showx2 x2)
            showy
      );
      correct name
    )
  )

let test_value_3 name ty reference showx1 showx2 showx3 showy eqy tests =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      tests |> List.iter (fun ((x1, x2, x3) as x) ->
        let actual = candidate x1 x2 x3
        and expected = reference x1 x2 x3 in
        if not (eqy actual expected) then
          mismatch name x actual expected
            3 (fun (x1, x2, x3) ->
              codebreak showx1 x1 @ codebreak showx2 x2 @ codebreak showx3 x3)
            showy
      );
      correct name
    )
  )

(* When doing black-box testing of a complete module, we are not testing just
   one function in isolation, but a group of functions together. In that case,
   the wording of the error message is somewhat different. Instead of saying
   that a specific function is incorrect, we want to say that an expression
   [expr] yields an incorrect result. *)

let black_box_compare
  (* Value equality and display, used to compare and show results. *)
  eq_value show_value
  (* Expression display. *)
  show_expr expr
  (* Actual behavior and expected behavior. *)
  actual_behavior
  expected_behavior
=
  let success =
    match actual_behavior, expected_behavior with
    | Ok actual, Ok expected ->
        eq_value actual expected (* value comparison *)
    | Error actual, Error expected ->
        actual = expected  (* exception comparison *)
    | Ok _, Error _
    | Error _, Ok _ ->
        false
  in
  (* Allow [TODO] to escape and abort the whole test. *)
  if actual_behavior = Error TODO then
    raise TODO;
  if not success then
    let header = [
      R.Text "Something is wrong.";
      R.Text "The following expression:";
      R.Break;
      R.Code (show_expr expr);
      R.Break;
    ]
    and actual =
      match actual_behavior with
      | Ok actual ->
          R.Text "produces the following value:" ::
          R.Output (show_value actual) ::
          []
      | Error actual ->
          R.Text "raises the following exception:" ::
          R.Output (Printexc.to_string actual) ::
          []
    and expected =
      match expected_behavior with
      | Ok expected ->
          R.Text "Yet, it should produce this value:" ::
          R.Output (show_value expected) ::
          []
      | Error expected ->
          R.Text "Yet, it should raise this exception:" ::
          R.Output (Printexc.to_string expected) ::
          []
    in
    fail (header @ actual @ expected)

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

(* An abstract syntax tree for expressions that construct integer sequences. *)

(* Because the Cartesian product of two integer sequences is a sequence of
   pairs of integers, we immediately use [Map (-)] to convert it back to a
   sequence of integers. We intentionally use a noncommutative operation
   (subtraction) so as to catch more mistakes. *)

(* We specialize [Map] to [Map (+1)]. *)

type expr =
  | EEmpty
  | ESingleton of int
  | ESum of expr * expr
  | EProductMapMinus of expr * expr
  | EMapSucc of expr

let esingleton x = ESingleton x
let esum (e1, e2) = ESum (e1, e2)
let eproduct (e1, e2) = EProductMapMinus (e1, e2)
let emap e = EMapSucc e

(* An enumeration of expressions. *)

let expr : expr Enum.enum =
  let open Enum in
  let elem : int enum = enum (Seq.up 0 2) in
  fix (fun expr ->
    just EEmpty ++
    map esingleton elem ++
    pay (
      map esum (expr ** expr) ++
      map emap expr ++
      map eproduct (expr ** expr)
    )
  )

(* An expression printer. *)

let rec print_atomic_expr e =
  group begin match e with
  | EEmpty ->
      apply "empty" []
  | _ ->
      parens (print_expr e)
  end

and print_expr e =
  group begin match e with
  | EEmpty ->
      print_atomic_expr e
  | ESingleton x ->
      apply "singleton" [ int x ]
  | ESum (e1, e2) ->
      apply "sum" [ print_atomic_expr e1; print_atomic_expr e2 ]
  | EProductMapMinus (e1, e2) ->
      apply "map (fun (x, y) -> x - y)" [
        parens (apply "product" [ print_atomic_expr e1; print_atomic_expr e2 ])
      ]
  | EMapSucc e ->
      apply "map succ" [ print_atomic_expr e ]
  end

let print_length_expr e =
  apply "length" [ print_atomic_expr e ]

let show_length_expr =
  wrap print_length_expr

let print_get_expr e i =
  apply "get" [ print_atomic_expr e; int i ]

let show_get_expr e =
  wrap (print_get_expr e)

let print_elements_expr e =
  apply "elements" [ print_atomic_expr e ]

let show_elements_expr =
  wrap print_elements_expr

(* An expression evaluator. *)

(* This evaluator is parameterized over an implementation of sequences. *)

(* Because (I think) we can only grab the student's code at a monomorphic
   type, we cannot require these functions to be polymorphic. We require
   just the monomorphic instances that we need for our test. *)

module type STUDENT = sig
  val empty: int seq
  val singleton: int -> int seq
  val sum: int seq -> int seq -> int seq
  val product: int seq -> int seq -> (int * int) seq
  val map: (int -> int) -> int seq -> int seq
  val map': (int * int -> int) -> (int * int) seq -> int seq
end

let solution =
  let module Solution = struct include Solution let map' = map end in
  (module Solution : STUDENT)

let eval student (e : expr) : int seq =
  let module Student = (val student : STUDENT) in
  let open Student in
  let rec eval e =
    match e with
    | EEmpty ->
        empty
    | ESingleton x ->
        singleton x
    | ESum (e1, e2) ->
        sum (eval e1) (eval e2)
    | EProductMapMinus (e1, e2) ->
        map' (fun (x, y) -> x - y) (product (eval e1) (eval e2))
    | EMapSucc e ->
        map succ (eval e)
  in
  eval e

(* -------------------------------------------------------------------------- *)

(* Black-box testing of a sequence implementation. *)

(* We proceed in three separate steps:
   - test sequence construction and [length].
   - test [get].
   - test [foreach]. *)

(* We test all construction expressions [e] up to the size threshold below. *)

let threshold =
  3 (* This represents 4728 tests. *)

(* Testing construction and [length]. *)

let test_length student length =
  for s = 0 to threshold do
    Seq.foreach (expr s) (fun e ->
      (* Check construction composed with [length]. *)
      let expected_seq = eval solution e in
      let expected_length = Solution.length expected_seq in
      let actual_behavior = T.result (fun () -> length (eval student e)) in
      black_box_compare (=) string_of_int show_length_expr e
        actual_behavior
        (Ok expected_length)
    )
  done;
  (* Success. *)
  let points = 1 in
  [ R.success points "The computation of a sequence's length seems correct." ]

(* Testing [get]. *)

let test_get student get =
  for s = 0 to threshold do
    Seq.foreach (expr s) (fun e ->
      (* We hope that the call [eval student e] does not raise an exception.
         If it is buggy, chances are that this was already caught above. *)
      let actual_seq = eval student e in
      let expected_seq = eval solution e in
      let expected_length = Solution.length expected_seq in
      (* Check [get] within bounds. *)
      for i = 0 to expected_length - 1 do
        let expected_behavior = Ok (Solution.get expected_seq i) in
        let actual_behavior = T.result (fun () -> get actual_seq i) in
        black_box_compare (=) string_of_int (show_get_expr e) i
          actual_behavior expected_behavior
      done;
      (* Check [get] outside bounds. *)
      let check i =
        let expected_behavior = Error OutOfBounds in
        let actual_behavior = T.result (fun () -> get actual_seq i) in
        black_box_compare (=) string_of_int (show_get_expr e) i
          actual_behavior expected_behavior
      in
      List.iter check [-1; -2; expected_length; expected_length + 1]
    )
  done;
  (* Success. *)
  let points = 1 in
  [ R.success points "Random access seems correct." ]

let test_foreach student foreach =
  for s = 0 to threshold do
    Seq.foreach (expr s) (fun e ->
      (* We hope that the call [eval student e] does not raise an exception.
         If it is buggy, chances are that this was already caught above. *)
      let actual_seq = eval student e in
      let expected_seq = eval solution e in
      let actual_behavior = T.result (fun () -> foreach actual_seq |> elements) in
      let expected_behavior = Ok (Solution.foreach expected_seq |> elements) in
      black_box_compare (=) show_list_int show_elements_expr e
        actual_behavior expected_behavior
    )
  done;
  (* Success. *)
  let points = 1 in
  [ R.success points "Iteration seems correct." ]

let test_sequences () =
  grab [%ty: int seq] "empty" (fun empty ->
  grab [%ty: int -> int seq] "singleton" (fun singleton ->
  grab [%ty: int seq -> int seq -> int seq] "sum" (fun sum ->
  grab [%ty: int seq -> int seq -> (int * int) seq] "product" (fun product ->
  grab [%ty: (int -> int) -> int seq -> int seq] "map" (fun map ->
  grab [%ty: (int * int -> int) -> (int * int) seq -> int seq] "map" (fun map' ->
    let module Student = struct
      let empty = empty
      let singleton = singleton
      let sum = sum
      let product = product
      let map = map
      let map' = map'
    end in
    let student = (module Student : STUDENT) in
    section "Question 2 (length)" (
      grab [%ty: int seq -> int] "length" (fun length ->
        protect (fun () ->
          test_length student length
        )
      )
    ) @
    section "Question 3 (random access)" (
      grab [%ty: int seq -> int -> int] "get" (fun get ->
        protect (fun () ->
          test_get student get
        )
      )
    ) @
    section "Question 4 (iteration)" (
      grab [%ty: int seq -> (int -> unit) -> unit] "foreach" (fun foreach ->
        protect (fun () ->
          test_foreach student foreach
        )
      )
    )
  ))))))

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_sequences() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
