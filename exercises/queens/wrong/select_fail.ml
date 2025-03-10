(* wrong: always fail *)
let select (xs : 'a list) : ('a * 'a list) m =
  fail
