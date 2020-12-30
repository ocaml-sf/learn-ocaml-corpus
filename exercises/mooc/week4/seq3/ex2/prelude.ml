type operation =
    Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list
