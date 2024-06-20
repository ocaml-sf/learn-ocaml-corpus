(* [select] intentionally missing, so this test is rejected *)

(* A correct [unfold] using fair conjunctions: *)

let rec unfold : type a s . (s -> bool) -> (s -> (a * s) m) -> s -> a list m =
  fun final step s ->
    if final s then
      return []
    else
      step s >>- fun (x, s) ->
      unfold final step s >>- fun xs ->
      return (x :: xs)
