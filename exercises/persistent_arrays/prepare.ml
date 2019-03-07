exception TODO

(* Override the dereferencing operator (!) so as to be able to
   count how many times it is used. Cross our fingers and hope
   that the student does not use the notation [r.contents]. *)

let (!), get_count =
  let count = ref 0 in
  let (!) r =
    incr count; !r
  and get_count() =
    !count
  in
  (!), get_count
