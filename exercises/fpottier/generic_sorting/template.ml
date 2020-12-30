let pigeonhole_sort (bound : int) (kvs : (int * 'v) list) : 'v list =
  (* TO DO: Define this function. *)
  raise TODO

let rec cmp : type a . a order -> a -> a -> result =
  fun o x y ->
    (* TO DO: Define this function. *)
    raise TODO

let rec sort : type k v . k order -> (k * v) list -> v list =
  fun o kvs ->
    (* TO DO: Define this function. *)
    raise TODO

let simple_sort (o : 'v order) (vs : 'v list) : 'v list =
  (* TO DO: Define this function. *)
  raise TODO

(* TO DO: Define [bool]. *)

(* TO DO: Define [list]. *)

(* TO DO: Define [string]. *)

(* TO DO: Define [int32]. *)

(* A discriminator returns a list of nonempty lists of values,
   where the inner lists group values whose keys are equivalent. *)

(* This is top-down discrimination (Henglein, 2012). *)

let rec discr : type k v . k order -> (k * v) list -> v list list =
  fun o kvs ->
    (* TO DO: Define this function. *)
    raise TODO
