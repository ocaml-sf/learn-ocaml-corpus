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

(* Assume that the list [xs] begins with the inorder traversal of a tree [t],
   followed with the element [mark], possibly followed itself with more
   elements [xs']. Assume that the list [ys] begins with the preorder
   traversal of the tree [t], possibly followed with more elements [ys'].
   Assume that the lists [xs] and [ys] have no duplicate elements. Then,
   [construct mark xs ys] must return the triple [(t, xs', ys')]. *)

let rec reconstruct mark xs ys =
  (* The list [xs] must be nonempty. *)
  assert (xs <> []);
  if hd xs = mark then
    (* [xs] begins with [mark]. The inorder traversal of the tree is
         empty, which implies that the tree must be a leaf. *)
    Leaf, xs, ys
  else begin
    (* [xs] does not begin with [mark], so the tree must be a node.
       Its root label [y] must be the first element of the list [ys],
       which cannot be empty. *)
    assert (ys <> []);
    let y, ys = hd ys, ys in (* wrong *)
    (* We can now identify the left subtree. Indeed, the element [y]
       serves as an end marker for its inorder traversal. *)
    let left, xs, ys = reconstruct y xs ys in
    (* The element [y] must now appear in front of [xs]. *)
    assert (hd xs = y);
    let xs = tl xs in
    (* We can now identify the right subtree. *)
    let right, xs, ys = reconstruct mark xs ys in
    (* We are done. *)
    Node (left, y, right), xs, ys
  end

let reconstruct (xs : 'a list) (ys : 'a list) : 'a tree =
  let mark = '$' in (* some element that does not occur in [xs] *)
  let t, xs, ys = reconstruct mark (xs @ [mark]) ys in
  assert (xs = [mark]);
  assert (ys = []);
  t
