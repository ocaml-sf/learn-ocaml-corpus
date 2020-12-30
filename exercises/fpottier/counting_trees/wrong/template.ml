(* TO DO: Define [weight] and [height]. *)

(* TO DO: Define [naive_trees_of_weight]. *)

(* TO DO: Define [trees_of_weights]. *)

(* The function call [fix ff] creates and returns a new memoizing function [f]
   of type [a -> b]. Its argument [ff] is itself a function, whose type is
   [(a -> b) -> (a -> b)], and which expects to receive [f] as an argument. *)

(* For example, a memoizing factorial can be defined using [fix] as
   follows:

    let fact =
      fix (fun self ->
        fun n ->
          if n = 0 then 1 else n * self(n-1)
      )

   Note that the definition of [fact] does not involve [let rec]. Instead,
   when we wish to perform a recursive call, we invoke [self], the memoizing
   function which [fix] has constructed for us. *)

(* We assume that it is permitted to use values of type [a] as keys in a hash
   table. That is, we assume that OCaml's generic equality and hash functions
   behave in a sensible way when applied to values of type [a]. Thus, we can
   use the functions [create], [add], [find] provided by the module [Hashtbl]
   in OCaml's standard library. *)

(* It is now up to you to implement [fix], based on the following skeleton. *)

let fix : type a b . ((a -> b) -> (a -> b)) -> (a -> b) =
  fun ff ->
    let table = Hashtbl.create 128 in
    let rec f (x : a) : b =
      (* TO DO: complete this code. *)
      raise TODO
    in
    f

(* TO DO: Define [sigma]. *)

(* TO DO: Define [split_weight]. *)

(* TO DO: Define [trees_of_weight]. *)

(* TO DO: Define [trees_of_weight_0_19]. *)

(* TO DO: Define [split_wb_weight]. *)

(* TO DO: Define [wb_trees_of_weight]. *)

(* TO DO: Define [wb_trees_of_weight_0_19]. *)
