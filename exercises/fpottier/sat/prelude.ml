type var = int

type formula =
  | FConst of bool
  | FConn  of bool * formula * formula
  | FNeg   of formula
  | FVar   of var

type env = var -> bool
