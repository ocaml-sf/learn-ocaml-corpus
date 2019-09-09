let display_image width height f =
  for y = 0 to height do
    for x = 0 to width do
      if f x y then print_char '#' else print_char ' '
    done ;
    print_newline ()
  done
;;

let rec render blend x y = match blend with
  | Image f -> f x y
  | Or (f, g) -> render f x y || render g x y
  | And (f, g) -> render f x y && render g x y
  | Rem (f, g) -> if render g x y then false else render f x y
;;

let display_blend width height blend =
  display_image width height (render blend)
;;
