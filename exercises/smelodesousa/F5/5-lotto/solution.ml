(* 1 *)
let make_grid n = Array.make_matrix n n false

(* 2 *)
type grids = bool array array

(* 3 *)
let grid = make_grid 7 
    
(* 4 *)
let fill l : grids = 
  let grid = make_grid 7 in
  let rec go_through_list l = 
    match l with 
    | []     -> ()
    | h :: t -> let i = (h - 1) / 7 in grid.(i).((h - 1) - (i * 7)) <- true; go_through_list t in
  go_through_list l;
  grid

(* 5 *)
let prize_draw (g : grids) l c = 
  let get_value n = 
    let i = (n - 1) / 7 in
    g.(i).((n - 1) - (i * 7)) in
  let rec get_output g l = 
    match l with
    | []     -> []
    | h :: t -> if get_value h then h :: (get_output g t) else get_output g t in
  (get_output g l, get_value c)