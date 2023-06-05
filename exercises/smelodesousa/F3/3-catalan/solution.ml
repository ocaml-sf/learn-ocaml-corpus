let rec catalan = function
  | 0 | 1 -> 1
  | n when n > 1 -> catalan (n - 1) * 2 * (2 * n - 1) / (n + 1) 
  | _ -> invalid_arg "catalan"