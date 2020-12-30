let pigeonhole_sort (bound : int) (kvs : (int * 'v) list) : 'v list =
  (* Create an array of buckets, each of which is initially empty. *)
  assert (0 <= bound);
  let bucket : 'v list array = Array.make bound [] in
  (* Place every value into an appropriate bucket, according to its key. *)
  List.iter (fun (k, v) ->
    assert (0 <= k && k < bound);
    bucket.(k) <- v :: bucket.(k)
  ) kvs;
  (* Concatenate all buckets. *)
  List.flatten (Array.to_list bucket)
