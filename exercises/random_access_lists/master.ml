(* BEGIN INCLUDE
(* TO DO: define the constant [empty]. *)

   END INCLUDE *)
(* BEGIN EXCLUDE *)
(* The empty random access list. *)

let empty : 'a . 'a seq =
  Nil

(*   END EXCLUDE *)
(* BEGIN INCLUDE
(* TO DO: define the constant [test24]. *)

(* TO DO: define the constant [digits]. *)

   END INCLUDE *)
(* BEGIN EXCLUDE *)
(* Example random access lists. *)

let test24 : int seq =
  Zero (One ((2, 4), Nil))

let digits : int seq =
  Zero (
  One ((0, 1),
  Zero (
  One ((((2, 3), (4, 5)), ((6, 7), (8, 9))),
  Nil
  ))))

(*   END EXCLUDE *)
(* Measuring the length of a sequence. *)

let rec length : 'a . 'a seq -> int =
  fun xs ->
(* BEGIN INCLUDE
    (* TO DO: implement [length]. *)
    raise TODO
   END INCLUDE *)
(* BEGIN EXCLUDE *)
    match xs with
    | Nil         ->                 0
    | Zero xs     ->     2 * length xs
    | One (_, xs) -> 1 + 2 * length xs
(*   END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: define the function [cons]. *)
   END INCLUDE *)
(* BEGIN EXCLUDE *)
(* Inserting an element in front of a sequence. *)

let rec cons : 'a . 'a -> 'a seq -> 'a seq =
  fun x ys ->
    match ys with
    | Nil ->
        One (x, Nil)
    | Zero ys ->
        One (x, ys)
    | One (y, ys) ->
        Zero (cons (x, y) ys)
(* END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: define the function [uncons]. *)
   END INCLUDE *)
(* BEGIN EXCLUDE *)
(* Extracting the head of a sequence. *)

let rec uncons : 'a . 'a seq -> ('a * 'a seq) option =
  fun xs ->
    match xs with
    | Nil ->
        None
    | One (x, Nil) ->
        Some (x, Nil)
    | One (x, ys) ->
        Some (x, Zero ys)
    | Zero ys ->
        match uncons ys with
        | Some ((x, y), ys) ->
            Some (x, One (y, ys))
        | None ->
            assert false (* cannot happen; no trailing zeros *)
(*   END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: define the function [get]. *)
   END INCLUDE *)
(* BEGIN EXCLUDE *)
(* Accessing the [i]-th element for reading. *)

let rec get : 'a . int -> 'a seq -> 'a =
  fun i xs ->
    match xs with
    | Nil ->
        assert false (* cannot happen; [i] is within bounds *)
    | One (x, xs) ->
        if i = 0 then
          x
        else
          get (i - 1) (Zero xs)
    | Zero xs ->
        let (x0, x1) = get (i / 2) xs in
        if i mod 2 = 0 then x0 else x1
(*   END EXCLUDE *)

(* BEGIN INCLUDE
(* TO DO: define the function [update]. *)
   END INCLUDE *)
(* BEGIN EXCLUDE *)
(* Accessing the [i]-th element for updating. *)

let rec fupdate : 'a . int -> ('a -> 'a) -> 'a seq -> 'a seq =
  fun i f xs ->
    match xs with
    | Nil ->
        assert false (* cannot happen; [i] is within bounds *)
    | One (x, xs) ->
        if i = 0 then
          One (f x, xs)
        else
          cons x (fupdate (i - 1) f (Zero xs))
    | Zero xs ->
        let f' =
          if i mod 2 = 0 then
            fun (x0, x1) -> (f x0, x1)
          else
            fun (x0, x1) -> (x0, f x1)
        in
        Zero (fupdate (i / 2) f' xs)

let update i y xs =
  fupdate i (fun _ -> y) xs
(* END EXCLUDE *)

(* An application of random access lists. *)

let rec eval (env : env) (e : expr) : constant =
  (* BEGIN INCLUDE
  (* TO DO: implement [eval]. *)
  raise TODO
       END INCLUDE *)
  (* BEGIN EXCLUDE *)
  match e with
  | EConstant c ->
      c
  | EBinOp (e1, op, e2) ->
      op (eval env e1) (eval env e2)
  | EVar x ->
      get x env
  | ELet (e1, e2) ->
      let env = cons (eval env e1) env in
      eval env e2
  (*   END EXCLUDE *)
