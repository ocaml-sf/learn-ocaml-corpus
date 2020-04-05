(* Beautiful inefficient definition: *)

let rec inorder t =
  match t with
  | Leaf ->
      []
  | Node (u, x, v) ->
      inorder u @ [x] @ inorder v

let rec preorder t =
  match t with
  | Leaf ->
      []
  | Node (u, x, v) ->
      [x] @ preorder u @ preorder v

(* Efficient definition: *)

let rec inorder t k =
  match t with
  | Leaf ->
      k
  | Node (u, x, v) ->
      inorder u (x :: inorder v k)

let inorder t =
  inorder t []

let rec preorder t k =
  match t with
  | Leaf ->
      k
  | Node (u, x, v) ->
      x :: preorder u (preorder v k)

let preorder t =
  preorder t []

(* [i1] and [i2] both have inorder traversal ['a'; 'b'; 'c']. *)

let i1, i2 =
  Node (Node (Node (Leaf, 'a', Leaf), 'b', Leaf), 'c', Leaf),
  Node (Node (Leaf, 'a', Node (Leaf, 'b', Leaf)), 'c', Leaf)

(* [p1] and [p2] both have preorder traversal ['a'; 'b'; 'c']. *)

let p1 =
  Node (Node (Node (Leaf, 'c', Leaf), 'b', Leaf), 'a', Leaf)

let p2 = p1
