type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let hd = List.hd
let tl = List.tl
