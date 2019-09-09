let rec equal_on_common l1 l2 = match l1,l2 with
    | [],_ -> true
    | _,[] -> true
    | h1::r1,h2::r2 -> h1=h2 && equal_on_common r1 r2;;
