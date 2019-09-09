let rec height = function
  | Empty -> 0
  | Node (t,_,t') -> 1 + (max (height t) (height t'))

let rec balanced = function
  | Empty -> true
  | Node(t,_,t') -> (height t)=(height t') && (balanced t) && (balanced t')
