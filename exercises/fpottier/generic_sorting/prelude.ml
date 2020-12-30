(* An OCaml encoding of the anonymous sum type ['a + 'b]. *)

(* The type [('a, 'b) either] is the sum of the types ['a] and ['b].
   A value of type [('a, 'b) either] is either [Inl v], where [v]
   has type ['a], or [Inr v], where [v] has type ['b]. The data
   constructors [Inl] and [Inr] are so named because they are the
   left injection and right injection into the sum type. *)

type ('a, 'b) either =
| Inl of 'a
| Inr of 'b

(* A value of type ['k order] is a description of a total preorder
   on values of type ['k]. *)

type _ order =
| OTrue:                                         'k order
| ONat:                                  int -> int order
| OSum : 'k1 order * 'k2 order -> ('k1, 'k2) either order
| OProd:       'k1 order * 'k2 order -> ('k1 * 'k2) order
| OMap :            ('k1 -> 'k2) * 'k2 order -> 'k1 order

(* A value of type [result] describes the result of a comparison. *)

type result = Lt | Eq | Gt
