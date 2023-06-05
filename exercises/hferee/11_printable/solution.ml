
let print (Show s) v = print_string (s v ^ "\n");;

let show_int = Show string_of_int;;
let show_float = Show string_of_float;;
let show_string = Show (fun s -> "\"" ^ s ^ "\"");;

let show_string_l = Show (fun s -> "\"" ^ s ^ "\"(" ^ string_of_int (String.length s) ^ ")");;

let show_pair (Show sx) (Show sy) =
  Show (fun (x, y) -> "(" ^ sx x ^ ", " ^ sy y ^ ")");;

let show_list (Show s) =
  let rec aux xs =
    match xs with
    | [] -> ""
    | [x] -> s x
    | x :: xs -> s x ^ "; " ^ aux xs
  in
  Show (fun xs -> "[ " ^ aux xs ^ " ]")
;;

let show_list_lines (Show s) =
  show_list (Show (fun a -> " " ^ s a ^ "\n"))

let show_list_list s =
  show_list_lines (show_list s)
