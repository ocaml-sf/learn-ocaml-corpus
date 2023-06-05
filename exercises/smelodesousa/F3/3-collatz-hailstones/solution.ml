let rec collatz = function
  | 1 -> [1]
  | n when n mod 2 = 0 -> n :: (collatz (n / 2))
  | n -> n :: (collatz ((3 * n) + 1))