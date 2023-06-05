let rec approximate_pi k =
  let n = float_of_int k in
  match n with
  | invalid when n <= 0. || n > 10000. -> raise (Failure "approximate_pi")
  | 1. -> 8. /. 3.
  | _ ->
      approximate_pi (k - 1)
      *. (n *. 2. *. (n *. 2.) /. (((n *. 2.) -. 1.) *. ((n *. 2.) +. 1.)))

(* Original version:
   let rec approximate_pi k =
     try
       let f = float_of_int k in
       if k < 1
       then raise (Failure "approximate_pi")
       else
         if k = 1
         then 2. *. (((f*.2.)*.(f*.2.))/.(((f*.2.)-.1.)*.((f*.2.)+.1.)))
         else (((f*.2.)*.(f*.2.))/.(((f*.2.)-.1.)*.((f*.2.)+.1.))) *. (approximate_pi (k-1))
     with
       Stack_overflow -> raise (Failure "approximate_pi") *)
