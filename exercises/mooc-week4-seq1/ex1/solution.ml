let rec compose = function
  | [] -> fun x -> x
  | f::r -> fun x -> f (compose r x);;

let rec fixedpoint f start delta =
  let next = f start in
  if abs_float (start-.next) < delta then
    start
  else
    fixedpoint f next delta;;
