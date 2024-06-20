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
      doc :: map (fun doc -> group (break 1 ^^ doc)) docs

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

let paresseux name =
  let message = [ R.Code name; R.Text "seems lazy."; ] in
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
  match actual_behavior with
  | Error (TODO as e) ->
      raise e
  | Error (T.Timeout limit) ->
      fail (
        announcement @ (* maybe a different announcement would be preferable? *)
        R.Text "The following expression:" ::
        R.Break ::
        R.Code (show_expr expr) ::
        R.Break ::
        R.Text (sprintf "requires more than %d seconds. This is abnormal." limit) ::
        []
      )
  | _ ->
      if not (eq_behavior eq_value actual_behavior expected_behavior) then
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

let print_list_list_int =
  print_list print_list_int

let show_list_list_int =
  wrap print_list_list_int

(* Special cases. *)

let show_list_pair_int_list_int =
  wrap (print_list (fun (x, xs) -> tuple [ int x; print_list_int xs ]))

(* -------------------------------------------------------------------------- *)

(* Comparing two multisets, represented as lists, amounts to sorting these
   lists before comparing them. *)

let eq_multiset (compare : 'a -> 'a -> int) : 'a list -> 'a list -> bool =
  fun xs ys ->
    List.sort compare xs = List.sort compare ys

(* -------------------------------------------------------------------------- *)

(* Truncating a sequence at an optional length. *)

let take (limit : int option) (xs : 'a Seq.t) : 'a Seq.t =
  match limit with
  | None ->
      xs
  | Some k ->
      Seq.take k xs

let print_take (limit : int option) (doc : document) : document =
  match limit with
  | None ->
      doc
  | Some k ->
      piped_apply "Seq.take" [ int k; doc ]

(* -------------------------------------------------------------------------- *)

(* Projecting a behavior. *)

let unok = function
  | Ok x -> x
  | Error _ -> assert false

(* -------------------------------------------------------------------------- *)

(* English hacks. *)

(* [english_count k name] produces a string that refers to [k] copies of
   an object [name]. *)

let english_count k name =
  match k with
  | 0 ->
      sprintf "no %s" name
  | 1 ->
      sprintf "one %s" name
  | _ ->
      sprintf "%d %ss" k name

(* [english_subcount verb j k] is very ad hoc. It produces a sentence
   fragment of the form " and <verb>s j of them", where "them" refers
   to [k] objects, and [j] is less than or equal to [k]. *)

let english_subcount verb j k =
  assert (j <= k);
  if j = k then
    match k with
    | 0 ->
        ""
    | 1 ->
        sprintf " and %ss it" verb
    | _ ->
        sprintf " and %ss all of them" verb
  else
    match j, k with
    | 0, 1 ->
        sprintf " and does not %s it" verb
    | 0, _ ->
        sprintf " and %ss none of them" verb
    | 1, _ ->
        sprintf " and %ss one of them" verb
    | _, _ ->
        sprintf " and %ss %d of them" verb j

(* -------------------------------------------------------------------------- *)

(* [select]. *)

(* We apply [select] to a list of integers of length [n],
   and we observe the first [n+1] results produced by the
   computation, which is enough, since we expect only [n]
   results. We compare the actual list of results against
   the expected list of results using a multiset comparison,
   as the order of appearance of the results is irrelevant. *)

let test_select select (xs : int list) =
  let n = List.length xs in
  T.result (fun () ->
    select xs |> sols |> Seq.take (n+1) |> Seq.to_list
  )

let print_test_select (xs : int list) =
  let n = List.length xs in
  piped_apply "Seq.to_list" [
    piped_apply "Seq.take" [ int (n+1);
      piped_apply "sols" [
        apply "select" [ print_list_int xs ]
      ]
    ]
  ]

let test_select_correctness select =
  let tests = Feat.(sample 100 3 (list (finite (up 0 3)))) in
  tests |> List.iter (fun xs ->
    let actual_behavior = test_select select xs
    and expected_behavior = test_select Solution.select xs in
    black_box_compare
      (eq_multiset compare) show_list_pair_int_list_int
      (incorrect "select")
      (wrap print_test_select) xs
      actual_behavior
      expected_behavior
  );
  correct "select"

(* We test whether [select] is lazy, that is, whether it uses [delay]
   appropriately. An insufficient use of [delay] leads to building a
   large computation, only part of which is actually executed. We test
   this by demanding the [k]-th result and checking that only [k]
   [choose] nodes are built and executed. This is highly fragile!
   e.g. replacing [choose] or [>>=] with their fair variants in the
   code of [select] causes the student code to be rejected! *)

let test_select_lazy select k (xs : int list) =
  T.result (fun () ->
    select xs |> sols |> Seq.take k |> Seq.to_list
  )

let print_test_select_lazy k (xs : int list) =
  piped_apply "Seq.to_list" [
    piped_apply "Seq.take" [ int k;
      piped_apply "sols" [
        apply "select" [ print_list_int xs ]
      ]
    ]
  ]

let test_select_laziness select =
  let tests = Feat.(sample 100 3 (list (finite (up 0 1)))) in
  tests |> List.iter (fun xs ->
    for k = 0 to List.length xs do
      reset();
      let _actual_behavior = test_select_lazy select k xs in
      let b, x = !BuildCount.choose, !ExecCount.choose in
      if b > k || x > k then
        fail (
          R.Code "select" :: R.Text "is not lazy." ::
          R.Break ::
          R.Text "The following expression:" ::
          R.Break ::
          R.Code (wrap (print_test_select_lazy k) xs) ::
          R.Break ::
          R.Text (sprintf
                    "should build and execute %s, but actually builds %s%s."
                    (english_count k "choice point")
                    (english_count b "choice point")
                    (english_subcount "execute" x b)
          ) ::
          []
        )
    done
  );
  paresseux "select"

let test_select () =
  section "Question 1" (
    grab [%ty: 'a list -> ('a * 'a list) m] "select" (fun select ->
      section "Functional correctness" (
        protect (fun () ->
          test_select_correctness select
        )
      ) -@> fun () ->
      section "Laziness" (
        protect (fun () ->
          test_select_laziness select
        )
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* [unfold]. *)

(* We test functional correctness only. Since there are no [delay]s in the
   solution for [unfold], I do not see how it could fail to be lazy. Even
   functional correctness is pretty hard to get wrong, in fact. There are
   only a couple mistakes that one can make. *)

(* I wish to allow the student to use either [>>=] or [>>-], so the order of
   the results must be considered irrelevant. The lists of results are
   compared as multisets. One difficulty that this creates is that the
   sequence produced by the student's code could be infinite, if there is a
   mistake in the code. We first evaluate the sequence produced by our
   solution, measure its length, and truncate the student's sequence at that
   length plus one. *)

(* The type of state and the functions [final] and [step] must be chosen so
   that reversing a valid list of choices does *not* yield another valid list
   of choices! Otherwise, a mistake where the student builds lists of choices
   in reverse order will not be detected; it will be mistaken with producing
   valid results in a different order. *)

let test_unfold unfold final step s limit =
  T.result (fun () ->
    unfold final step s |> sols |> take limit |> Seq.to_list
  )

let print_test_unfold final step s limit =
  piped_apply "Seq.to_list" [
    print_take limit (
      piped_apply "sols" [
        apply "unfold" [ utf8string final; utf8string step; int s ]
      ]
    )
  ]

let test_unfold_correctness unfold =
  let finals = [
     is_zero, "is_zero";
  ]
  and steps = [
     decrement, "decrement";
     decrease, "decrease";
  ] in
  finals |> List.iter (fun (final, final_desc) ->
    steps |> List.iter (fun (step, step_desc) ->
      for s = 0 to 4 do
        let expected_behavior = test_unfold Solution.unfold final step s None in
        let limit = Some (List.length (unok expected_behavior) + 1) in
        let actual_behavior = test_unfold unfold final step s limit in
        black_box_compare
          (eq_multiset compare) show_list_list_int
          (incorrect "unfold")
          (wrap (print_test_unfold final_desc step_desc s)) limit
          actual_behavior
          expected_behavior
      done
    )
  );
  correct "unfold"

let test_unfold () =
  section "Question 2" (
    grab [%ty: ('s -> bool) -> ('s -> ('a * 's) m) -> 's -> 'a list m]
    "unfold" (fun unfold ->
      protect (fun () ->
        test_unfold_correctness unfold
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* [permutations]. *)

let test_permutations permutations xs limit =
  T.result (fun () ->
    permutations xs |> sols |> take limit |> Seq.to_list
  )

let print_test_permutations xs limit =
  piped_apply "Seq.to_list" [
    print_take limit (
      piped_apply "sols" [
        apply "permutations" [ print_list_int xs ]
      ]
    )
  ]

let test_permutations_correctness permutations =
  for s = 0 to 5 do
    let xs = up 0 s in
    let expected_behavior = test_permutations Solution.permutations xs None in
    (* The length of the list [unok expected_behavior] should be [fact s]. *)
    let limit = Some (List.length (unok expected_behavior) + 1) in
    let actual_behavior = test_permutations permutations xs limit in
    black_box_compare
      (eq_multiset compare) show_list_list_int
      (incorrect "permutations")
      (wrap (print_test_permutations xs)) limit
      actual_behavior
      expected_behavior
  done;
  correct "permutations"

let test_permutations () =
  section "Question 3" (
    grab [%ty: 'a list -> 'a list m] "permutations" (fun permutations ->
      protect (fun () ->
        test_permutations_correctness permutations
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* [nodup]. *)

let test_nodup () =
  let tests = Feat.(sample 50 4 (list (finite (up 0 4)))) in
  section "Question 4" (
    test_value_1 "nodup" [%ty: 'a list -> bool] Solution.nodup
      print_list_int
      show_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* [safe]. *)

(* Our test cases are partial placements of [k] out of [n] queens. *)

let placements k n : placement m =
  assert (0 <= k && k <= n);
  Solution.unfold
    (fun xs -> List.length xs = n - k)
    Solution.select
    (up 0 n)

let placements (k, n) : placement list =
  placements k n |> sols |> Seq.to_list

let test_safe () =
  let tests : placement list =
    up 0 5
    |> List.map (fun n -> [ (n/2, n); (n, n) ])
    |> List.flatten
    |> List.map placements
    |> List.flatten
  in
  section "Question 5" (
    test_value_1 "safe" [%ty: placement -> bool] Solution.safe
      print_list_int
      show_bool (=)
      tests
  )

(* -------------------------------------------------------------------------- *)

(* [queens]. *)

let test_queens queens n limit =
  T.result (fun () ->
    queens n |> sols |> take limit |> Seq.to_list
  )

let print_test_queens n limit =
  piped_apply "Seq.to_list" [
    print_take limit (
      piped_apply "sols" [
        apply "queens" [ int n ]
      ]
    )
  ]

let test_queens_correctness name queens max_n =
  for n = 0 to max_n do
    let expected_behavior = test_queens Solution.queenz n None in
    let limit = Some (List.length (unok expected_behavior) + 1) in
    let actual_behavior = test_queens queens n limit in
    black_box_compare
      (eq_multiset compare) show_list_list_int
      (incorrect name)
      (wrap (print_test_queens n)) limit
      actual_behavior
      expected_behavior
  done;
  correct name

let test_queens q name max_n =
  section q (
    grab [%ty: int -> placement m] name (fun queens ->
      protect (fun () ->
        test_queens_correctness name queens max_n
      )
    )
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let report () =
  test_select() @
  test_unfold() @
  test_permutations() @
  test_nodup() @
  test_safe() @
  (* queens 7 works in batch mode, causes stack overflow in the browser *)
  test_queens "Question 6" "queens" 7 @
  (* queenz 10 works in batch mode, causes stack overflow in the browser *)
  test_queens "Question 7" "queenz" 10 @
  []

let () =
  T.set_result (T.ast_sanity_check code_ast report)
