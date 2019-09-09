let my_example = EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3))

let rec eval = function
  | EInt x -> x
  | EAdd (lhs, rhs) -> eval lhs + eval rhs
  | EMul (lhs, rhs) -> eval lhs * eval rhs

let factorize = function
  | EAdd (EMul (a, b), EMul (a', c)) when a = a' ->
    EMul (a, EAdd (b, c))
  | e ->
    e

let expand = function
  | EMul (a, EAdd (b, c)) ->
    EAdd (EMul (a, b), EMul (a, c))
  | e ->
    e

let simplify = function
  | EMul (EInt 0, e) | EMul (e, EInt 0) -> EInt 0
  | EAdd (EInt 0, e) | EAdd (e, EInt 0)
  | EMul (EInt 1, e) | EMul (e, EInt 1) -> e
  | e -> e
