(* Basic constructor functions. *)

(* TO DO: Define [empty]. *)

let just (x : 'a) : 'a enum =
  (* TO DO: Complete this definition. *)
  raise TODO

let pay (enum : 'a enum) : 'a enum =
  (* TO DO: Complete this definition. *)
  raise TODO

let sum (enum1 : 'a enum) (enum2 : 'a enum) : 'a enum =
  (* TO DO: Complete this definition. *)
  raise TODO

let ( ++ ) =
  sum

let product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
  (* TO DO: Complete this definition. *)
  raise TODO

let ( ** ) =
  product

let map (phi : 'a -> 'b) (enum : 'a enum) : 'b enum =
  (* TO DO: Complete this definition. *)
  raise TODO

(* Derived constructor functions. *)

(* TO DO: Define [bit]. *)

let list (elem : 'a enum) : 'a list enum =
  (* TO DO: Complete this definition. *)
  raise TODO

(* TO DO: Define [tree]. *)

let balanced_product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
  (* TO DO: Complete this definition. *)
  raise TODO

let ( *-* ) =
  balanced_product

(* TO DO: Define [balanced_tree]. *)
