let rec tribonacci n =
  match n with
  | invalid when n < 0 -> raise (Invalid_argument "tribonacci")
  | 0 -> 1
  | 1 -> 1
  | 2 -> 1
  | n -> tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

let tribonacci_iter n =
  match n with
  | invalid when n < 0 -> raise (Invalid_argument "tribonacci")
  | 0 | 1 | 2 -> 1
  | _ ->
      let a = ref 1 in
      let b = ref 1 in
      let c = ref 1 in
      for i = 3 to n do
        let aux = !a + !b + !c in
        a := !b;
        b := !c;
        c := aux
      done;
      !c

let rec tribonacci_tail n a b c =
  match n with
  | invalid when n < 0 -> raise (Invalid_argument "tribonacci")
  | 0 | 1 | 2 -> c
  | _ -> tribonacci_tail (n - 1) b c (a + b + c)

(* Original version:
   let rec tribonacci n =
     if n <= 2 then 1
     else (tribonacci (n-1)) + (tribonacci (n-2)) + (tribonacci (n-3))

   let tribonacci_iter n =
     let n1 = ref 1 in
     let n2 = ref 1 in
     let n3 = ref 1 in

     for i=3 to n do
       let tmp = !n3 in
       n3 := !n1 + !n2 + !n3;
       n1 := !n2;
       n2 := tmp;
     done;
     !n3

   let rec tribonacci_tail n a b c =
     if n <= 2 then c
     else (tribonacci_tail[@tailcall]) (n-1) b c (a+b+c) *)
