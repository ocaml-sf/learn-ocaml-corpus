let empty : heap =
  E

let singleton x =
  T(1, x, T(1, x, E, E), E)
