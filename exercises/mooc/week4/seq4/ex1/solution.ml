
let wrap l = List.map (function x -> [x]) l;;

let rec tree_map f = function
  | Node(left,x,right) -> Node(tree_map f left,f x,tree_map f right)
  | Leaf x -> Leaf (f x)
;;
