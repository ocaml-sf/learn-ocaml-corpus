type 'a t = {
  (* A physical array. *)
  mutable table: 'a array;
}

let default_size =
  1024

let make x = {
  table = Array.make default_size x;
}

let get a i =
  if i < Array.length a.table then
    a.table.(i)
  else
    a.table.(0) (* wrong *)

let set a i x =
  if i < Array.length a.table then
    a.table.(i) <- x
