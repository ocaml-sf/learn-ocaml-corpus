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

let p1, p2 =
  Node (Node (Node (Leaf, 'c', Leaf), 'b', Leaf), 'a', Leaf),
  Node (Node (Leaf, 'b', Node (Leaf, 'c', Leaf)), 'a', Leaf)

let rec split n ys =
  if n = 0 then
    [], ys
  else
    let y, ys = hd ys, tl ys in
    let ys1, ys2 = split (n - 1) ys in
    y :: ys1, ys2

let rec reconstruct ys =
  if ys = [] then
    Leaf
  else
    (* The tree must be a node. Its root label [y] must be the first
       element of the list [ys], which cannot be empty. *)
    let y, ys = hd ys, tl ys in
    (* We can now split [ys] in an arbitrary way and continue! *)
    let n = List.length ys in
    let ys1, ys2 = split (n/2) ys in
    Node (reconstruct ys1, y, reconstruct ys2)

let reconstruct (xs : 'a list) (ys : 'a list) : 'a tree =
  reconstruct ys
