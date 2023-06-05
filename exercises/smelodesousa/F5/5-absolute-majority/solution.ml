(* 1 *)
let check_voter hashtable n number_candidates = 
    try ( let count = Hashtbl.find hashtable n in
        Hashtbl.replace hashtable n (count+1);)
    with Not_found ->
        ( Hashtbl.add hashtable n 1;
        number_candidates := !number_candidates + 1;)

let majority array_votos = 
    let my_hash = Hashtbl.create 10 in
    let number_candidates = ref(0) in 
    let majr = ref(0) in
    let number = ref (0) in
    let majority_candidate_list = ref [] in
    begin
        for i=0 to (Array.length array_votos) - 1 do
            check_voter my_hash (array_votos.(i)) number_candidates;
        done;
        for i=1 to !number_candidates do 
            number := Hashtbl.find my_hash i;
            if !majr = !number then majority_candidate_list := !majority_candidate_list @ [i]
            else if !majr < !number then ( majr := !number; majority_candidate_list := [i] )
            else ();
        done;
        if (List.length !majority_candidate_list) >= 2 then raise Not_found
        else List.nth !majority_candidate_list 0;
    end

(* 2 *)
let mjrty (a: ((int) list)) : int =
    let exception QtReturn of (int) in
    try
      let n = List.length a in
      let cand = ref (List.nth a 0) in
      let k = ref 1 in
      (let o = n - 1 in let o1 = 1 in
       for i = o1 to o do
         if !k = 0
         then begin cand := List.nth a i; k := 1 end
         else begin if !cand = List.nth a i then incr k else decr k end
       done);
      if !k = 0 then raise (Not_found);
      if !k > n / 2 then raise (QtReturn !cand);
      k := 0;
      (let o = n - 1 in let o1 = 0 in
       for i1 = o1 to o do
         if List.nth a i1 = !cand
         then begin incr k; if !k > n / 2 then raise (QtReturn !cand) end
       done);
      raise (Not_found)
    with
    | QtReturn r -> r