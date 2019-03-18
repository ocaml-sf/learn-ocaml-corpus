type _ seq =
| Empty    : 'a seq
| Singleton: 'a -> 'a seq
| Sum      : int * 'a seq * 'a seq -> 'a seq
| Product  : int * 'a seq * 'b seq -> ('a * 'b) seq
| Map      : int * ('a -> 'b) * 'a seq -> 'b seq

exception OutOfBounds

let elements (foreach : ('a -> unit) -> unit) : 'a list =
  let xs = ref [] in
  foreach (fun x -> xs := x :: !xs);
  List.rev !xs
