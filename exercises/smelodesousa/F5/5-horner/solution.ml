(* 1 *)
(* Multiple-choice question *)
let answer = B

(* 2 *)
let horner x p =
  let rec aux x ret = function
    | h :: t -> aux x ((ret *. x) +. h) t
    | [] -> ret
  in
  match p with
  | d, h :: t when d = List.length t -> aux x h t
  | _ -> raise (Invalid_argument "horner")

(* 3 *)
let derivative p =
  let rec aux n ret = function
    | [ _ ] -> List.rev ret
    | h :: t -> aux (n -. 1.) ((h *. n) :: ret) t
    | [] -> raise (Invalid_argument "horner")
  in
  match p with
  | d, pol when d = List.length pol - 1 -> (d - 1, aux (float_of_int d) [] pol)
  | _ -> raise (Invalid_argument "horner")

(* 4 *)
let rec derivative_tr p acc =
  match acc with
  | d1, p1 -> (
      match p with
      | d2, pol when d2 <> List.length pol - 1 ->
          raise (Invalid_argument "horner")
      | _, [] -> raise (Invalid_argument "horner")
      | _, [ _ ] -> (d1, List.rev p1)
      | d2, h :: t ->
          derivative_tr (d2 - 1, t) (d1 + 1, (float_of_int d2 *. h) :: p1))

(* Original version:
   (* 2 *)
   let horner x_value polynomial =
     let result = ref(0.0) in
       match polynomial with
       |(a,b) -> List.iter (fun y -> result := !result *. x_value +. y) b;
     !result
   (* 3 *)
   let derivative polynomial =
     let resulting_polynomial = ref [] in
     match polynomial with
     |(a,b)->(let i = ref (List.length b - 1) in
               List.iter (fun y -> (if !i <> 0 then (resulting_polynomial := !resulting_polynomial @ [(float_of_int !i) *. y]; i := !i - 1;))) b;
             ((List.length !resulting_polynomial - 1),!resulting_polynomial))

   (* 4 Potentially incorrect type (not polynomial -> polynomial -> polynomial)*)
   let derivative_polynomial = ref []

   let rec derivative_tr polynomial =
     match polynomial with
     |(a,b) -> if a = 0 then ((List.length !derivative_polynomial - 1),!derivative_polynomial)
               else match b with
                   |h::t -> (derivative_polynomial := !derivative_polynomial @ [h *. float_of_int a];
                             derivative_tr (a-1,t)) *)
