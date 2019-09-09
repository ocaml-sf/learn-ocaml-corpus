let rec height = function
  | Empty -> 0
  | Node (t, _, t') -> 1 + (max (height t) (height t')) ;;

let rec balanced = function
  | Empty -> true
  | Node (t, _, t') ->
      (balanced t) && (balanced t') && height t = height t' ;;

let bal_height bst =
  "Replace this string with your implementation." ;;

let balanced_fast bst =
  "Replace this string with your implementation." ;;
