let make (n : int) (x : 'a) : 'a parray =
(* BEGIN INCLUDE
  (* TO DO: implement [make]. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  let data = Array.make n x in
  ref (Array { data })
(*   END EXCLUDE *)

let set (base : 'a parray) (i : int) (value : 'a) =
(* BEGIN INCLUDE
  (* TO DO: implement [set]. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  ref (Apply { base; i; value })
(*   END EXCLUDE *)

let rec revert (a : 'a parray) : 'a array =
(* BEGIN INCLUDE
  (* TO DO: implement [revert]. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  match !a with
  | Array { data } ->
      data
  | Apply { base; i; value } ->
      let data = revert base in
      let v = data.(i) in
      data.(i) <- value;
      a := Array { data };
      base := Apply { base = a; i; value = v };
      data
(*   END EXCLUDE *)

let get (a : 'a parray) (i : int) : 'a =
(* BEGIN INCLUDE
  (* TO DO: implement [get]. *)
  raise TODO
(*   END INCLUDE *)
(* BEGIN EXCLUDE *)
  let data = revert a in
  data.(i)
(*   END EXCLUDE *)
