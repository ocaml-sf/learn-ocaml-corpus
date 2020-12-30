let pigeonhole_sort (bound : int) (kvs : (int * 'v) list) : 'v list =
  (* Create an array of buckets, each of which is initially empty. *)
  assert (0 <= bound);
  let bucket : 'v list array = Array.make bound [] in
  (* Place every value into an appropriate bucket, according to its key. *)
  List.iter (fun (k, v) ->
    assert (0 <= k && k < bound);
    bucket.(k) <- v :: bucket.(k)
  ) kvs;
  (* Reverse the list stored in every bucket, so as to have a stable sort. *)
  for k = 0 to bound - 1 do
    bucket.(k) <- List.rev bucket.(k)
  done;
  (* Concatenate all buckets. *)
  List.flatten (Array.to_list bucket)

let rec cmp : type a . a order -> a -> a -> result =
  fun o x y ->
    match o with
    | OTrue ->
        Eq
    | ONat _bound ->
        if x < y then Lt
        else if x = y then Eq
        else Gt
    | OSum (o1, o2) ->
        begin match x, y with
        | Inl x, Inl y ->
            cmp o1 x y
        | Inr x, Inr y ->
            cmp o2 x y
        | Inl _, Inr _ ->
            Lt
        | Inr _, Inl _ ->
            Gt
        end
    | OProd (o1, o2) ->
        let (x1, x2) = x
        and (y1, y2) = y in
        begin match cmp o1 x1 y1 with
        | Eq ->
            cmp o2 x2 y2
        | Lt | Gt as result ->
            result
        end
    | OMap (f, o) ->
        cmp o (f x) (f y)

let rec partition (kvs : (('a, 'b) either * 'v) list) : ('a * 'v) list * ('b * 'v) list =
  match kvs with
  | [] ->
      [], []
  | (Inl l, v) :: kvs ->
      let lvs, rvs = partition kvs in
      (l, v) :: lvs, rvs
  | (Inr r, v) :: kvs ->
      let lvs, rvs = partition kvs in
      lvs, (r, v) :: rvs

let curryr ((k1, k2), v) =
  (k2, (k1, v))

let rec sort : type k v . k order -> (k * v) list -> v list =
  fun o kvs ->
    match o, kvs with
    | _, [] ->
        (* The input list is empty. Return an empty output list. *)
        []
    | _, [(_k, v)] ->
        (* The input list is a singleton. Return a singleton output list.
           This is just an optimization (this case could be removed), but
           it seems worthwhile, as it allows us to avoid the case analysis
           on [o], and the deconstruction of the value [v] that would come
           with it. *)
        [v]
    | OTrue, _ ->
        (* All keys are equivalent. Return just the values. *)
        List.map snd kvs
    | ONat bound, _ ->
        (* The keys are integers. Use pigeonhole sort. *)
        pigeonhole_sort bound kvs
    | OSum (o1, o2), _ ->
        (* Split the key-value list in two sublists: those whose key is
           [Inl _], and those whose key is [Inr _]. Sort each of them,
           and concatenate them, as [Inl _] is less than [Inr _]. *)
        let lvs, rvs = partition kvs in
        sort o1 lvs @ sort o2 rvs
    | OProd (o1, o2), _ ->
        (* To sort lexicographically with respect to keys which are pairs,
           sort with respect to the least significant pair component first,
           then sort with respect to the most significant pair component.
           The fact that this is a stable sort is crucial. *)
        kvs
          |> List.map curryr
          |> sort o2
          |> sort o1
    | OMap (f, o), _ ->
        (* Apply [f] to every key and sort. *)
        sort o (List.map (fun (k, v) -> (f k, v)) kvs)

let simple_sort (o : 'v order) (vs : 'v list) : 'v list =
  sort o (List.map (fun v -> (v, v)) vs)

let bool : bool order =
  OMap ((fun b -> if b then 1 else 0), ONat 2)
  (* In this solution, [bool] is mapped into the integer interval [0..1].
     Another approach would be to map it to the sum type [1 + 1]. *)

let unfold (xs : 'a list) : (unit, 'a * 'a list) either =
  match xs with
  | [] ->
      Inl ()
  | x :: xs ->
      Inr (x, xs)

let list (o : 'a order) : 'a list order =
  (* A recursive value definition. *)
  let rec lexico = OMap (unfold, OSum (OTrue, OProd (o, lexico))) in
  lexico

let char : char order =
  OMap (Char.code, ONat 64) (* wrong: bound is too low *)

(* A string is converted to a list of characters as follows. *)

(* One could arrange for the conversion to take place on demand,
   so the conversion is carried out only so far as required by
   the comparison. For the sake of simplicity, this is not done
   here. *)

let chars (s : string) : char list =
  let n = String.length s in
  let c = Array.init n (String.get s) in
  Array.to_list c

let string : string order =
  OMap (chars, list char)
