
let rec equal_on_common = function
  | [] -> (function _ -> true)
  | h1::r1 -> function
      | [] -> true
      | h2::r2 -> h1 = h2 && equal_on_common r1 r2;;
