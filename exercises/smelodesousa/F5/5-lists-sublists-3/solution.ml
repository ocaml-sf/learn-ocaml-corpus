open List

let answer1 = [[]]

let answer2 = [['c']]

let answer3 = [['b'; 'c']; ['c'; 'b']]

let rec insertion e l = match l with
    []    -> [[e]]
  | x::xs -> (e::l) :: (map (fun li -> x::li) (insertion e xs))

let rec permutation l = match l with
    []    -> [[]]
  | x::xs ->  flatten (map (fun l -> (insertion x l)) (permutation xs))
