type 'a seq = {
  length : int;
  get    : int -> 'a;
  foreach: ('a -> unit) -> unit;
}

exception OutOfBounds

let elements (foreach : ('a -> unit) -> unit) : 'a list =
  let xs = ref [] in
  foreach (fun x -> xs := x :: !xs);
  List.rev !xs
