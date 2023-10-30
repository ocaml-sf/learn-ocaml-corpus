open Random

let knuth_shuffle a =
  Array.iteri
    (fun i e ->
      let j = Random.int (i + 1) in
      a.(i) <- a.(j);
      a.(j) <- e)
    a;
  a

(* Alternate version with a regular loop:
   let knuth_shuffle v =
     for i = (Array.length v) - 1 downto 1 do
       let j = Random.int (i+1) in
       let aux = v.(i) in
       v.(i) <- v.(j);
       v.(j) <- aux
     done;
     v
*)
