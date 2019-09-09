let rotate a =
  let n = Array.length a in 
  if n > 1 then 
    let v = a.(0) in
    for i = 0 to n-2 do
      a.(i) <- a.(i+1)
    done;
    a.(n-1)<-v ;;

(* silly solution, totally inefficient when n is big,
   but that's just for testing the results, ok :-) *)
let rotate_by a n =
  let len = Array.length a in 
  if len > 1 then 
    let rotateone a =
      let v = a.(0) in
      for i = 0 to len-2 do
        a.(i) <- a.(i+1)
      done;
      a.(len-1)<-v in
    let rec nmod n =
      if n < 0 then nmod (n + len)
      else if n >= len then n - len
      else n in
    for i = 1 to nmod n do rotateone a done ;;
