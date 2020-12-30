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

let concat (docs : document list) : document =
  List.fold_right (^^) docs empty

let comma =
  utf8string "," ^^ break 1

let pre_comma doc =
  comma ^^ doc

let pre_commas docs =
  concat (map pre_comma docs)

let commas docs =
  match docs with
  | [] ->
      empty
  | doc :: docs ->
      doc ^^ pre_commas docs

let int i =
  utf8string (string_of_int i)

let parens doc =
  utf8string "(" ^^ doc ^^ utf8string ")"

let parens doc =
  parens (nest 2 (break 0 ^^ doc) ^^ break 0)

let tuple docs =
  group (parens (commas docs))

let construct label docs =
  utf8string label ^^ space ^^ tuple docs

let wrap (print : 'a -> document) : 'a -> string =
  fun x -> pretty 70 (group (print x))

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

(* -------------------------------------------------------------------------- *)

(* Some code is taken from the solution. *)

open Solution

(* -------------------------------------------------------------------------- *)

(* Well-formedness of random access lists. *)

let rec wf : 'a . 'a seq -> bool =
  fun xs ->
    match xs with
    | Nil ->
        true
    | Zero Nil ->
        false
    | Zero xs
    | One (_, xs) ->
        wf xs

(* -------------------------------------------------------------------------- *)

(* Converting a random access list to a list. *)

(* This takes linear time, I think. *)

type ('a, 'b) cons =
  'a -> 'b list -> 'b list

let pair (cons : ('a, 'b) cons) : ('a * 'a, 'b) cons =
  fun (x, y) bs ->
    cons x (cons y bs)

let rec elements : 'a 'b . ('a, 'b) cons -> 'a seq -> 'b list =
  fun cons xs ->
    match xs with
    | Nil         -> []
    | Zero    xs  ->         elements (pair cons) xs
    | One (x, xs) -> cons x (elements (pair cons) xs)

let elements (xs : 'a seq) : 'a list =
  elements (fun x xs -> x :: xs) xs

(* -------------------------------------------------------------------------- *)

(* A printer for random access lists. *)

let rec print_seq : 'a . ('a -> document) -> 'a seq -> document =
  fun print_element xs ->
    match xs with
    | Nil ->
        utf8string "Nil"
    | Zero xs ->
        let print_pair (x, y) = tuple [print_element x; print_element y] in
        construct "Zero" [ print_seq print_pair xs ]
    | One (x, xs) ->
        let print_pair (x, y) = tuple [print_element x; print_element y] in
        construct "One" [ print_element x; print_seq print_pair xs ]

let print_int_seq =
  print_seq int

let show_int_seq =
  wrap print_int_seq

(* A printer for the type [(int * int seq) option]. *)

let print_iiso =
  print_option (fun (x, xs) ->
    tuple [ int x; print_seq int xs ]
  )

let show_iiso =
  wrap print_iiso

(* A printer for integer sequences -- at the level of the model. *)

let print_model (xs : int list) : document =
  group (commas (map int xs))

let show_model =
  wrap print_model

(* A printer for environments. *)

let show_env env =
  assert (env = empty);
  "empty"

(* A printer for arithmetic expressions. *)

let print_op op =
  utf8string (
    match op 12 12 with
    | 144 ->
        "( * )"
    | 0 ->
        "( - )"
    | 24 ->
        "( + )"
    | _ ->
        "( ? )" (* should not happen *)
  )

let rec print_expr e =
  match e with
  | EConstant k ->
      construct "EConstant" [ int k ]
  | EVar x ->
      construct "EVar" [ int x ]
  | EBinOp (e1, op, e2) ->
      construct "EBinOp" [ print_expr e1; print_op op; print_expr e2 ]
  | ELet (e1, e2) ->
      construct "ELet" [ print_expr e1; print_expr e2 ]

let show_expr =
  wrap print_expr

(* -------------------------------------------------------------------------- *)

(* A simple way of constructing random access lists. *)

let import (xs : 'a list) : 'a seq =
  List.fold_right cons xs empty

(* Generating a random integer list of length [i]. *)

let list i : int list =
  T.Sampler.sample_list ~min_size:i ~max_size:i
    (fun () -> Random.int 10)
    ()

(* Test cases. *)

let tests : int seq list =
  64 |> deepening (fun i ->
    [list i |> import]
  )

(* -------------------------------------------------------------------------- *)

(* Example sequences. *)

(* [test_seq_constant] is pretty much like [test_value_0], but produces a
   better report by checking well-formedness first, then checking if the
   model is the intended model. *)

let test_seq_constant name ty reference eq =
  T.test_value (T.lookup_student ty name) (fun candidate ->
    protect (fun () ->
      if not (wf candidate) then
        fail [
          R.Code name; R.Text "is ill-formed. Remember, ";
          R.Code "Zero"; R.Text " cannot be followed with ";
          R.Code "Nil"; R.Text ".";
        ];
      if not (eq candidate reference) then
        fail [
          R.Code name; R.Text "is incorrect. It represents the sequence:";
          R.Output (show_model (elements candidate));
        ];
      let message = [ R.Code name; R.Text "is correct."; ] in
      [ R.Message (message, R.Success 1) ]
    )
  )

let test_examples () =
  section "Question 1" (
    test_seq_constant "empty" [%ty : int seq] empty (=)
  ) @
  section "Question 2" (
    test_seq_constant "test24" [%ty: int seq] test24 (=)
  ) @
  section "Question 3" (
    test_seq_constant "digits" [%ty: int seq] digits (=)
  ) @
  []

(* -------------------------------------------------------------------------- *)

(* Length. *)

let test_length () =
  section "Question 4" (
    test_value_1 "length" [%ty: int seq -> int] length
      show_int_seq show_int (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Cons. *)

let test_cons () =
  let tests =
    tests |> map (fun xs -> Random.int 10, xs)
  in
  section "Question 5" (
    test_value_2 "cons" [%ty: int -> int seq -> int seq] cons
      show_int show_int_seq show_int_seq (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Uncons. *)

let test_uncons () =
  section "Question 6" (
    test_value_1 "uncons" [%ty: int seq -> (int * int seq) option] uncons
      show_int_seq show_iiso (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Get. *)

let test_get () =
  let tests =
    tests |> flat_map (fun xs ->
      let n = length xs in
      up 0 n |> map (fun i ->
        i, xs
      )
    )
  in
  section "Question 7" (
    test_value_2 "get" [%ty: int -> int seq -> int] get
      show_int show_int_seq show_int (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Update. *)

let test_update () =
  let tests =
    tests |> flat_map (fun xs ->
      let n = length xs in
      up 0 n |> map (fun i ->
        i, 42, xs
      )
    )
  in
  section "Question 9" (
    test_value_3 "update" [%ty: int -> int -> int seq -> int seq] update
      show_int show_int show_int_seq show_int_seq (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Eval. *)

let test_eval () =
  let ( + ) e1 e2 = EBinOp (e1, ( + ), e2) in
  let ( - ) e1 e2 = EBinOp (e1, ( - ), e2) in
  let ( * ) e1 e2 = EBinOp (e1, ( * ), e2) in
  let c i = EConstant i in
  let v x = EVar x in
  let tests = [
    c 23;
    c 12 + c 12;
    ELet (c 12, v 0 * v 0);
    ELet (c 4, ELet (c 8, v 0 - v 1));
    ELet (
      ELet (c 4, ELet (c 8, v 0 - v 1)),
      v 0 + v 0
    )
  ] |> map (fun e -> empty, e) in
  section "Question 10" (
    test_value_2 "eval" [%ty: env -> expr -> constant] eval
      show_env show_expr show_int (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_examples() @
  test_length() @
  test_cons() @
  test_uncons() @
  test_get() @
  test_update() @
  test_eval() @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
