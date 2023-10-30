let answer1 = [[]]

let answer2 = [[]; ['c']]

let answer3 = [[]; ['c']; ['b']; ['b'; 'c']]

let rec sublist l = 
  match l with
  [] -> [[]]
  | h::t -> let sl = (sublist t) in sl @  (List.map (fun l -> h::l) sl)