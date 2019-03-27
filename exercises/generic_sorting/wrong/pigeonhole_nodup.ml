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
  (* Keep only one element per bucket. *) (* wrong *)
  for k = 0 to bound - 1 do
    if bucket.(k) <> [] then
      bucket.(k) <- [List.hd bucket.(k)]
  done;
  (* Concatenate all buckets. *)
  List.flatten (Array.to_list bucket)
