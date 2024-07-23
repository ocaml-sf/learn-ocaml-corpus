let used_cos = ref 0
let used_mul = ref 0
let used_div = ref 0

let set r = incr r
let unset r = r := 0

let ccr_reference a b c s =
  s /. (2. *. (cos (a /. 2.)) *. 2. *. (cos (b /. 2.)) *. 2. *. (cos (c /. 2.)))

module Stdlib = struct
  include Stdlib
  let cos_f = cos
  let cos v =
    set used_cos;
    cos_f v

  let mult_f = ( *. )
  let ( *. ) x y =
    set used_mul;
    mult_f x y

  let div_f = ( /. )
  let ( /. ) x y =
    set used_div;
    div_f x y
end

include Stdlib

let test_ccr ccr (a, b, c, s) =
  let unset_all () =
    unset used_cos;
    unset used_mul;
    unset used_div in
  let total () =
    !used_cos + !used_mul + !used_div in
  let ok () = !used_cos = 1 && (!used_div + !used_mul >= 2) in
  unset_all ();
  let ccr_a = ccr a in
  let ok_a = ok () in
  let total_a = total () in
  unset_all ();
  let ccr_b = ccr_a b in
  let ok_b = ok () in
  let total_b = total () in
  unset_all ();
  let ccr_c = ccr_b c in
  let ok_c = ok () in
  let total_c = total () in
  unset_all ();
  let _ = ccr_c s in
  let ok_s = !used_div + !used_mul = 1 && !used_cos = 0 in
  ok_a, ok_b, ok_c, ok_s, total () + total_a + total_b + total_c
