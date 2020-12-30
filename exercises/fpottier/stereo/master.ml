(* BEGIN EXCLUDE
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

     END EXCLUDE *)
let inorder t =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  inorder t []

let rec preorder t k =
  match t with
  | Leaf ->
      k
  | Node (u, x, v) ->
      x :: preorder u (preorder v k)
     END EXCLUDE *)

let preorder t =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  preorder t []

(* [i1] and [i2] both have inorder traversal ['a'; 'b'; 'c']. *)
     END EXCLUDE *)

let i1, i2 =
(* BEGIN INCLUDE *)
  (* TO DO: Define these constants. *)
  Leaf,
  Leaf
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  Node (Node (Node (Leaf, 'a', Leaf), 'b', Leaf), 'c', Leaf),
  Node (Node (Leaf, 'a', Node (Leaf, 'b', Leaf)), 'c', Leaf)

(* [p1] and [p2] both have preorder traversal ['a'; 'b'; 'c']. *)
     END EXCLUDE *)

let p1, p2 =
(* BEGIN INCLUDE *)
  (* TO DO: Define these constants. *)
  Leaf,
  Leaf
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  Node (Node (Node (Leaf, 'c', Leaf), 'b', Leaf), 'a', Leaf),
  Node (Node (Leaf, 'b', Node (Leaf, 'c', Leaf)), 'a', Leaf)

(* Assume that the list [xs] begins with the inorder traversal of a tree [t],
   followed with the element [sentinel], possibly itself followed with more
   elements [xs']. Assume that the list [ys] begins with the preorder
   traversal of the tree [t], possibly followed with more elements [ys'].
   Assume that the lists [xs] and [ys] have no duplicate elements. Then,
   [reconstruct_aux sentinel xs ys] must return
   the triple [(t, sentinel :: xs', ys')]. *)

let rec reconstruct_aux sentinel xs ys =
  (* The list [xs] must be nonempty. *)
  assert (xs <> []);
  if hd xs = sentinel then
    (* [xs] begins with [sentinel]. The inorder traversal of the tree is
         empty, which implies that the tree must be a leaf. *)
    Leaf, xs, ys
  else begin
    (* [xs] does not begin with [sentinel], so the tree must be a node.
       Its root label [y] must be the first element of the list [ys],
       which cannot be empty. *)
    assert (ys <> []);
    let y, ys = hd ys, tl ys in
    (* We can now identify the left subtree. Indeed, the element [y]
       serves as an end marker (a sentinel) for its inorder traversal. *)
    let left, xs, ys = reconstruct_aux y xs ys in
    (* The element [y] must now appear in front of [xs]. *)
    assert (hd xs = y);
    let xs = tl xs in
    (* We can now identify the right subtree. *)
    let right, xs, ys = reconstruct_aux sentinel xs ys in
    (* We are done. *)
    Node (left, y, right), xs, ys
  end
     END EXCLUDE *)

let reconstruct (xs : 'a list) (ys : 'a list) : 'a tree =
(* BEGIN INCLUDE *)
  (* TO DO: Define this function. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE
  let sentinel = '$' in (* some element that does not occur in [xs] *)
  let t, xs, ys = reconstruct_aux sentinel (xs @ [sentinel]) ys in
  assert (xs = [sentinel]);
  assert (ys = []);
  t
     END EXCLUDE *)
