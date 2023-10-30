let fractals n =
  let rec isPow2 = function
    | 1 -> true
    | n -> if n mod 2 = 0 then isPow2 (n / 2) else false
  in
  let rec print_stars = function
    | n ->
        if n > 1 then (
          print_string "* ";
          print_stars (n - 1))
        else print_endline "*"
  in
  let rec print_spaces = function
    | n ->
        if n > 0 then (
          print_string "  ";
          print_spaces (n - 1))
        else print_string ""
  in
  let rec aux n spaces =
    if n > 0 then (
      aux (n / 2) spaces;
      print_spaces spaces;
      print_stars n;
      aux (n / 2) (spaces + (n / 2)))
  in
  match n with
  | invalid when n < 1 || n > 100 || not (isPow2 n) ->
      raise (Invalid_argument "fractals")
  | _ -> aux n 0

(* NOTE: On the original solution (below) there's an empty space before new lines ("* * \n").
   On the proposed solution above, that's not the case. ("* *\n") *)

(* Original version:
   let rec valid n =
     match n with
     | 1 -> true
     | _ -> match (n mod 2) with 0 -> valid (n/2) | _ -> false

   let rec fractals n =
     let rec  fractals_aux left length =

       if length = 0
       then ()
       else
         begin

           fractals_aux left (length/2);

           for i=0 to (left-1) do Printf.printf "  " done;
           for i=0 to (length-1) do Printf.printf "* " done;
           Printf.printf "\n";

           fractals_aux (left + (length/2)) (length/2)
         end in
       if (valid n) then fractals_aux 0 n else raise (Invalid_argument "fractals") *)
