(* -- Part A -------------------------------------------------------------- *)

let words str =
  let buf = Buffer.create 20 in
  let res = ref [] in
  let push () =
    match Buffer.contents buf with
    | "" -> ()
    | word ->
        Buffer.clear buf ;
        res := word :: !res in
  for i = 0 to String.length str - 1 do
    match String.get str i with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c ->
        Buffer.add_char buf c
    | _ ->
        push ()
  done ;
  push () ;
  List.rev !res

let build_ltable words =
  let rec add pre suf acc = function
    | (pre', sufs) :: rest when pre = pre' ->
        let acc = (pre, suf :: sufs) :: acc in
        List.rev_append acc rest
    | ass :: rest -> add pre suf (ass :: acc) rest
    | [] -> (pre, [ suf ]) :: acc in
  let rec loop acc = function
    | [] -> acc
    | [ last ] -> add last "STOP" [] acc
    | pre :: (suf :: _ as rest) -> loop (add pre suf [] acc) rest in
  loop [] ("START" :: words) ;;

let next_in_ltable table word =
  let sufs = List.assoc word table in
  List.nth sufs (Random.int (List.length sufs)) ;;

let walk_ltable table =
  let rec loop cur acc =
    match next_in_ltable table cur with
    | "STOP" -> List.rev acc
    | word -> loop word (word :: acc) in
  loop "START" []

(* -- Part B -------------------------------------------------------------- *)

let compute_distribution l =
  let rec dist = function
    | [], acc, len -> { total = len ; amounts = acc }
    | w :: rest, [], len -> dist (rest, [ (w, 1) ], len + 1)
    | w :: rest, (w', n) :: acc, len when w = w' -> dist (rest, (w, n + 1) :: acc, len + 1)
    | w :: rest, acc, len -> dist (rest, (w, 1) :: acc, len + 1) in
  dist (List.sort compare l, [], 0)

let build_htable words =
  let lht = Hashtbl.create 100 in
  let ladd pre suf =
    Hashtbl.replace lht pre
        (suf :: match Hashtbl.find lht pre with l -> l | exception Not_found -> []) in
    let rec loop = function
      | [] -> ()
      | [ last ] -> ladd last "STOP"
      | pre :: (suf :: _ as rest) -> ladd pre suf ; loop rest in
    loop ("START" :: words) ;
    let oht = Hashtbl.create 100 in
    let oadd pre l = Hashtbl.add oht pre (compute_distribution l) in
    Hashtbl.iter oadd lht ;
    oht

let next_in_htable table word =
  let { total = n ; amounts = dist } =
    Hashtbl.find table word in
  let rec find n = function
    | [] -> raise Not_found
    | (w, n') :: _ when n < n' -> w
    | (_, n') :: rest -> find (n - n') rest in
  find (Random.int n) dist

let walk table =
  let rec loop cur acc =
    match next_in_htable table cur with
    | "STOP" -> List.rev acc
    | word -> loop word (word :: acc) in
  loop "START" []

let walk_htable table =
  let rec loop cur acc =
    match next_in_htable table cur with
    | "STOP" -> List.rev acc
    | word -> loop word (word :: acc) in
  loop "START" []

(* -- Part C -------------------------------------------------------------- *)

let sentences str =
  let buf = Buffer.create 20 in
  let res = ref [] in
  let cur = ref [] in
  let push () =
    match Buffer.contents buf with
    | "" -> ()
    | word ->
        Buffer.clear buf ;
        cur := word :: !cur in
  let next () =
    if !cur <> [] then begin
      res := List.rev !cur :: !res ;
      cur := []
    end in
  for i = 0 to String.length str - 1 do
    match String.get str i with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\128' .. '\255' as c ->
        Buffer.add_char buf c
    | ';' | ',' | ':' | '-' | '"' | '\'' as c ->
        push () ;
        Buffer.add_char buf c ;
        push ()
    | '?' | '!' | '.' as c ->
        push () ;
        Buffer.add_char buf c ;
        push () ;
        next ()
    | _ ->
        push ()
  done ;
  push () ;
  next () ;
  List.rev !res

let rec start lp =
  if lp = 0 then [] else "START" :: start (lp - 1)

let shift l x =
  List.tl l @ [ x ]

let build_ptable words lp =
  let lht = Hashtbl.create 100 in
  let ladd pre suf =
    Hashtbl.replace lht pre
      (suf :: match Hashtbl.find lht pre with l -> l | exception Not_found -> []) in
  let rec loop acc = function
    | [] -> ladd acc "STOP"
    | suf :: rest ->
        ladd acc suf ;
        loop (shift acc suf) rest in
  loop (start lp) words ;
  let oht = Hashtbl.create 100 in
  let oadd pre l = Hashtbl.add oht pre (compute_distribution l) in
  Hashtbl.iter oadd lht ;
  { table = oht ; prefix_length = lp }

let walk_ptable { table ; prefix_length = lp } =
  let rec loop cur acc =
    match next_in_htable table cur with
    | "STOP" -> if Random.bool () then loop (start lp) acc else List.rev acc
    | word -> loop (shift cur word) (word :: acc) in
  loop (start lp) []

let merge_ptables tl =
  let tr = Hashtbl.create 100 in
  let pl = ref 0 in
  let merge_one { table = t ; prefix_length = p } =
    begin if !pl = 0 then pl := p else assert (p = !pl) end ;
    Hashtbl.iter
      (fun k { total = s ; amounts = d } ->
         Hashtbl.add tr k @@
         match Hashtbl.find tr k with
         | exception Not_found ->
             { total = s ; amounts = d }
         | { total = s' ; amounts = d' } ->
             { total = s + s' ; amounts = d @ d' })
      t in
  List.iter merge_one tl ;
  { table = tr ; prefix_length = !pl }
