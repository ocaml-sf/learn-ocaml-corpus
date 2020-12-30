let rec print_int_list = function
  | [] -> ()
  | i :: r -> print_int i; print_newline (); print_int_list r

let rec print_list print = function
  | [] -> ()
  | i :: r -> print i; print_newline (); print_list print r

let print_every_other k l =
  ignore
    (List.fold_left
       (fun count el ->
          if (count mod k) = 0
          then begin print_int el; print_newline (); 1 end
          else count+1)
       0 l)
