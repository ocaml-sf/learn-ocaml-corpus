let rotate a =
  let n = Array.length a in 
  let v = a.(0) in
  for i = 0 to n-2 do
    a.(i) <- a.(i+1)
  done;
  a.(n-1)<-v ;;

let rotate_by a n =
  "Replace this string with your implementation." ;;
