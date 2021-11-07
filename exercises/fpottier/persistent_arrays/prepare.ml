exception TODO

(* Override the dereferencing operator (!) so as to be able to
   count how many times it is used. Cross our fingers and hope
   that the student does not use the notation [r.contents]. *)

let count = ref 0
let get_count () = !count
let (!) r = incr count; !r
let count = 0 (* for shadowing purposes *)
