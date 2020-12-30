(* Beautiful inefficient definition: *)

let rec inorder t =
  match t with
  | Leaf ->
      []
  | Node (u, x, v) ->
      inorder u @ [x] @ inorder v

let rec preorder t =
  match t with
  | Leaf ->
      []
  | Node (u, x, v) ->
      [x] @ inorder u @ inorder v (* wrong *)
