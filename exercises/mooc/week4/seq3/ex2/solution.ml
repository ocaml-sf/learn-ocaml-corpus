let rec lookup_function n = function
    [] -> invalid_arg "lookup_function"
  | (op, f) :: rem ->
      if n = op then f else lookup_function n rem

let add_function name op env = (name, op) :: env

let my_env = add_function "min" (fun x y -> if x < y then x else y) initial_env

let rec compute (env : env) op =
  match op with
    Op (n, op1, op2) ->
      let f = lookup_function n env in
      let v1 = compute env op1 in
      let v2 = compute env op2 in
      f v1 v2
  | Value v -> v

let rec compute_eff env = function
    Value v -> v
  | Op (n, op1, op2) ->
      lookup_function n env (compute_eff env op1) (compute_eff env op2)
