type 'a tree =
    Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;
