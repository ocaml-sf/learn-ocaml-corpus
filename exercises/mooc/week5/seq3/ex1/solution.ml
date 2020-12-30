let output_multiples x n m =
  for i = n to m do
    if is_multiple i x then begin
      print_int i ;
      print_string ","
    end
  done
;;

exception Zero

let display_sign_until_zero f n =
  try
    for i = 0 to n do
      if f i > 0 then print_endline "positive"
      else if f i < 0 then print_endline "negative"
      else raise Zero
    done
  with Zero -> print_endline "zero"
;;
