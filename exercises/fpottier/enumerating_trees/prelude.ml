type tree =
  | Leaf
  | Node of tree * tree

type labeled_tree =
  | LLeaf of int
  | LNode of labeled_tree * labeled_tree
