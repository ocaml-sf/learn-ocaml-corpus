let rec print_path path =
  "Replace this string with your implementation." ;;

let rec print_file lvl name =
  "Replace this string with your implementation." ;;

let rec print_symlink lvl name path =
  "Replace this string with your implementation." ;;

let rec print_dir lvl name =
  "Replace this string with your implementation." ;;

let print_filesystem root =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec print_filesystem lvl items =
    "Replace this string with your implementation." in
  print_filesystem 0 root ;;

let rec resolve sym path =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec resolve acc path =
    "Replace this string with your implementation."  in
  resolve (List.tl (List.rev sym)) path ;;

let rec file_exists root path =
  "Replace this string with your implementation." ;;

(* move print_filesystem here for exercise 8 *)
