type 'a t = {
  (* The default element that was passed at creation time. *)
  default: 'a;
  (* A physical array. *)
  mutable table: 'a array;
}

let default_size =
  16 (* must be non-zero *)

let make x = {
  default = x;
  table = Array.make default_size x;
}

let get a i =
  assert (0 <= i);
  if i < Array.length a.table then
    a.table.(i)
  else
    a.default

let rec new_length length i =
  if i < length then
    length
  else
    new_length (2 * length) i

let set a i x =
  assert (0 <= i);
  let length = Array.length a.table in
  if i < length then
    a.table.(i) <- x
  else begin
    let length' = new_length (2 * length) i in
    let table' = Array.make length' a.default in
    Array.blit a.table 0 table' 0 length;
    table'.(i) <- x;
    (* a.table <- table' (* wrong *) *)
  end
