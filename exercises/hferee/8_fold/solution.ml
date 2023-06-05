let list_min l = fold_right min l max_int

let list_max l = fold_right max l min_int


let count_if p l = fold_right (fun x acc -> acc + if p x then 1 else 0) l 0

let forall p l = fold_right (fun e acc -> p e && acc) l true

let exists p l = fold_right (fun e acc -> p e || acc) l false

let mem x = exists (( = ) x)


(* bonus *)
let find_first p l = fold_right (fun e acc -> if p e then 0 else if acc = - 1
                                  then -1 else acc + 1) l (-1)

let find_last p l = fold_right (fun e acc -> if acc >= 0 then acc + 1 else if p e
                                then 0 else -1) l (-1)


