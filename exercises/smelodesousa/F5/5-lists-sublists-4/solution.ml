let rec sublists = function 
  | []      -> [[]]
  | x :: xs ->
    let subl = (sublists xs) in
      subl @ (List.map (fun l -> x::l) subl)
            
let rec insertions e l = match l with
  | []     -> [[e]]
  | x :: xs -> (e :: l) :: (List.map (fun li -> x :: li) (insertions e xs))
                       
let rec permutations l = match l with
  | []    -> [[]]
  | x::xs -> List.concat (List.map (fun l -> (insertions x l)) (permutations xs))
                
let subbag l =
  List.sort compare
    (List.concat
       (List.map (permutations) (sublists l)));;
