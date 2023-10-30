let answer1 = [['e']]

let answer2 = [['e'; 'c']; ['c'; 'e']]

let answer3 = [['e'; 'b'; 'c']; ['b'; 'e'; 'c']; ['b'; 'c'; 'e']]

let rec insertion e l = match l with
    []    -> [[e]]
  | x::xs -> (e::l) :: (List.map (fun li -> x::li) (insertion e xs))
