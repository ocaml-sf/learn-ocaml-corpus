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
        cmp o (f y) (f x) (* wrong: reversed *)
