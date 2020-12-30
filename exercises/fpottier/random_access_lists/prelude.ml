(* The algebraic data type of random access lists. *)

type 'a seq =
| Nil
| Zero of     ('a * 'a) seq
| One of 'a * ('a * 'a) seq

(* An algebraic data type of arithmetic expressions. *)

type constant =
  int

type var =
  int (* a de Bruijn index *)

type op =
  int -> int -> int

type expr =
  | EConstant of constant
  | EBinOp of expr * op * expr
  | EVar of var
  | ELet of expr * expr

type env =
  constant seq
