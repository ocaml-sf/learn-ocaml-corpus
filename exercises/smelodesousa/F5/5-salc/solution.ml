(* 1 *)
let sort1 a =
  Array.sort (fun a b -> if a > b then -1 else if a < b then 1 else 0) a;
  a

(* 2 *)
let sort2 l =
  let rec aux odd even = function
    | h :: t ->
        if h mod 2 <> 0 then aux (h :: odd) even t else aux odd (h :: even) t
    | [] -> (odd, even)
  in
  let o, e = aux [] [] l in
  List.sort (fun a b -> if a > b then -1 else if a < b then 1 else 0) o
  @ List.sort (fun a b -> if a > b then 1 else if a < b then -1 else 0) e

(* 3 *)
let sort3 a =
  let rec aux a i odd even =
    if i = -1 then (odd, even)
    else if a.(i) mod 2 <> 0 then aux a (i - 1) (a.(i) :: odd) even
    else aux a (i - 1) odd (a.(i) :: even)
  in
  let l_o, l_e = aux a (Array.length a - 1) [] [] in
  let o, e = (Array.of_list l_o, Array.of_list l_e) in
  Array.sort (fun a b -> if a > b then -1 else if a < b then 1 else 0) o;
  Array.sort (fun a b -> if a > b then 1 else if a < b then -1 else 0) e;
  Array.append o e

(* 4 *)
let sort4 l =
  let rec int_to_digits l = function
    | 0 -> List.rev l
    | n -> int_to_digits ((n mod 10) :: l) (n / 10)
  in
  let sort_fun a b =
    let rec aux da db =
      match (da, db) with
      | ha :: ta, hb :: tb ->
          if ha > hb then 1 else if ha < hb then -1 else aux ta tb
      | [], _ :: _ -> -1
      | _ :: _, [] -> 1
      | [], [] -> 0
    in
    let digA = int_to_digits [] a in
    let digB = int_to_digits [] b in
    aux digA digB
  in
  List.sort sort_fun l

(* Original version:
   (* 1 *)
   let myCompare x y = if x < y then 1 else -1

   let sort1 a =
       Array.sort myCompare a;
       a

   (* 2 *)
   let myCompare_Odd x y = if x < y then 1 else -1

   let myCompare_Even x y = if x < y then -1 else 1

   let sort2 l : int list =
       let l_Even = ref [] and l_Odd = ref [] in
       begin
           List.iter (fun x -> if (x mod 2) = 0 then
               l_Even := !l_Even @ [x]
           else
               l_Odd := !l_Odd @ [x]) l;

           l_Odd := List.sort myCompare_Odd !l_Odd;
           l_Even := List.sort myCompare_Even !l_Even;
           !l_Odd @ !l_Even
       end

   (* 3 *)
   let myCompare_Odd x y = if x < y then 1 else -1

   let myCompare_Even x y = if x < y then -1 else 1

   let sort3 a : int array =
       let l = ref [] in
       let l_Even = ref [] and l_Odd = ref [] in
       begin
           l := Array.to_list a;
           List.iter (fun x -> if (x mod 2) = 0 then
               l_Even := !l_Even @ [x]
           else
               l_Odd := !l_Odd @ [x]) !l;

           l_Odd := List.sort myCompare_Odd !l_Odd;
           l_Even := List.sort myCompare_Even !l_Even;
           Array.of_list (!l_Odd @ !l_Even)
       end

   (* 4 *)
   let digits n =
     let rec loop n acc =
       if n = 0 then acc
       else loop (n/10) (n mod 10::acc) in
     match n with
     | 0 -> [0]
     | _ -> loop n []

   let myCompare x y =
     let x_list = List.rev (digits x) and y_list = List.rev (digits y) in
     let value = ref(0) and flag = ref(true) in
     let big_number = if List.length x_list > List.length y_list then 1 else -1 in
     for i=0 to ((min (List.length x_list) (List.length y_list)) - 1) do
       if List.nth x_list i > List.nth y_list i && !flag then ( value := 1; flag := false )
       else if List.nth x_list i < List.nth y_list i && !flag then ( value := -1; flag := false );
     done; if !value = 0 then big_number else !value

   let sort4 l =
     List.sort myCompare l
*)
