let rec height : 'a bt -> int * int = function
  | Empty -> 0, 1
  | Node(t,_,t') ->
      let h, n = height t
      and h', n' = height t' in
      1 + (max h h'), n + n'

let rec balanced : 'a bt -> bool * int = function
  | Empty -> true, 1
  | Node(t,_,t') ->
      let b, n = balanced t in
      if not b then
        false, n
      else
        let b',n' = balanced t' in
        if not (b && b') then
	  false, n + n'
        else
          let h, c = height t
          and h', c'= height t' in
          (h = h', n + n' + c + c')

(* For the solution, we try with both traversal orders. *)
let min_visited = ref 0 (* ouh, naughty *)
let max_visited = ref 10000 (* ouh, naughty *)
let bal_height : 'a bt -> int * int = fun t ->
  let rec bal_height_lr acc = function
    | Empty -> 0, acc + 1
    | Node(t,_,t') ->
        let h, acc = bal_height_lr acc t in
        let h', acc = bal_height_lr acc t' in
        if h = h' then h + 1, acc
        else raise (Unbalanced acc) in
  let rec bal_height_rl acc = function
    | Empty -> 0, acc + 1
    | Node(t,_,t') ->
        let h, acc = bal_height_rl acc t' in
        let h', acc = bal_height_rl acc t in
        if h = h' then h + 1, acc
        else raise (Unbalanced acc) in
  let rec bal_height_lr' acc = function
    | Empty -> 0, acc + 1
    | Node(Node _,_, Empty) -> raise (Unbalanced (acc + 1))
    | Node(Empty,_,Node _) -> raise (Unbalanced (acc + 1))
    | Node(t,_,t') ->
        let h, acc = bal_height_lr' acc t in
        let h', acc = bal_height_lr' acc t' in
        if h = h' then h + 1, acc
        else raise (Unbalanced acc) in
  let rec bal_height_rl' acc = function
    | Empty -> 0, acc + 1
    | Node(Node _,_, Empty) -> raise (Unbalanced (acc + 1))
    | Node(Empty,_,Node _) -> raise (Unbalanced (acc + 1))
    | Node(t,_,t') ->
        let h, acc = bal_height_rl' acc t' in
        let h', acc = bal_height_rl' acc t in
        if h = h' then h + 1, acc
        else raise (Unbalanced acc) in
  let bal_height_lr'' acc = function
    | Empty -> 0, acc + 1
    | Node(t,_,t') ->
        let rec height2 acc = function
          | Empty, Empty -> 0, acc + 2
          | Node (ll, _, lr), Node (rl, _, rr) ->
              let _, acc = height2 acc (ll, rl) in
              let h, acc = height2 acc (lr, rr) in
              h + 1, acc
          | _ -> raise (Unbalanced (acc + 1)) in
        height2 acc (t, t') in
  let bal_height_rl'' acc = function
    | Empty -> 0, acc + 1
    | Node(t,_,t') ->
        let rec height2 acc = function
          | Empty, Empty -> 0, acc + 2
          | Node (ll, _, lr), Node (rl, _, rr) ->
              let _, acc = height2 acc (lr, rr) in
              let h, acc = height2 acc (ll, rl) in
              h + 1, acc
          | _ -> raise (Unbalanced (acc + 1)) in
        height2 acc (t, t') in
  let lr = try `Ok (bal_height_lr 0 t) with Unbalanced n -> `Unbalanced n in
  min_visited := (match lr with `Ok (_, n) | `Unbalanced n -> n) ;
  max_visited := (match lr with `Ok (_, n) | `Unbalanced n -> n) ;
  List.iter
    (fun bal_height ->
       let n = try snd (bal_height 0 t) with Unbalanced n -> n in
       min_visited := min !min_visited n ;
       max_visited := max !max_visited n)
    [ bal_height_rl ;
      bal_height_lr' ;
      bal_height_rl';
      bal_height_lr'' ;
      bal_height_rl'' ] ;
  match lr with
  | `Ok (h, n) ->
      (h, n)
  | `Unbalanced n ->
      raise (Unbalanced n)

let balanced_fast : 'a bt -> bool * int = fun t ->
  try
    let (_,n) = bal_height t
    in true, n
  with Unbalanced n -> false, n
