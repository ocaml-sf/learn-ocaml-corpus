type 'a t = {
  (* A physical array. *)
  mutable table: 'a array;
}

let default_size =
  0 (* wrong *)

let make x = {
  table = Array.make default_size x;
}

let rec new_length length i =
  if i < length then
    length
  else
    new_length (2 * length) i

let ensure a i =
  assert (0 <= i);
  let table = a.table in
  let length = Array.length table in
  if i >= length then begin
    let default = a.table.(0) in
    let table' = Array.make (new_length (2 * length) i) default in
    Array.blit table 0 table' 0 length;
    a.table <- table'
  end

let get a i =
  ensure a i;
  a.table.(i)

let set a i x =
  ensure a i;
  a.table.(i) <- x
