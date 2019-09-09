let rec loop p f x =
  if p x then x else loop p f (f x)

let exists p l =
  let rec aux = function [] -> false | x :: xs -> if p x then true else aux xs in
  aux l

let find p l =
  let rec aux = function
    | [] -> raise NotFound
    | x :: xs when p x -> x
    | _ :: xs -> aux xs
  in
  aux l

let near x = [ x - 2 ; x - 1 ; x ; x + 1 ; x + 2 ]

let flat_map r =
  let rec aux accu = function [] -> accu | x :: xs -> aux (r x @ accu) xs in
  aux []

let rec iter_rel rel n x =
  if n <= 1 then rel x
  else flat_map rel (iter_rel rel (n - 1) x)

let solve r p x =
  find p (loop (exists p) (flat_map r) [x])

let solve_path r p x =
  List.rev @@
  solve
    (fun l -> List.map (fun y -> y :: l) (r (List.hd l)))
    (fun l -> p (List.hd l))
    [x]

let archive_map opset r (s, l) =
  let rec aux (s', l') = function
    | [] ->
        (s', List.flatten l')
    | x :: xs ->
        let rx = List.filter (fun c -> not (opset.mem c s')) (r x) in
        let s' = List.fold_right opset.add rx s' in
        aux (s', rx :: l') xs
  in
  aux (s, []) l

let solve' opset r p x =
  find p (snd (loop (fun (_, l) -> exists p l) (archive_map opset r) (opset.add x opset.empty, [x])))

let solve_path' opset r p x =
  List.rev @@
  solve' opset
    (fun l -> List.map (fun y -> y :: l) (r (List.hd l)))
    (fun l -> p (List.hd l))
    [x]

let solve_puzzle p opset c =
  solve_path'
    opset
    (fun c -> List.map (p.move c) (p.possible_moves c))
    p.final
    c

let final = function
  | [| [| (_,_) ; (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (S,0) ; (S,0) ; (_,_) |] ;
       [| (_,_) ; (S,0) ; (S,0) ; (_,_) |] |] -> true
  | _ -> false

let move_piece =
  let clone_board board = Array.map Array.copy board
  and out row col = row < 0 || row >= 5 || col < 0 || col >= 4
  in fun init_board p { drow; dcol } ->
    let board = clone_board init_board in
    try
      for row = 0 to 4 do
	for col = 0 to 3 do
	  if init_board.(row).(col) = p then board.(row).(col) <- x
	done
      done;
      for row = 0 to 4 do
	for col = 0 to 3 do
	  if init_board.(row).(col) = p then (
	    let row' = row + drow and col' = col + dcol in
	    if out row' col' || board.(row').(col') <> x then raise Not_found;
	    board.(row').(col') <- p
	  )
	done
      done;
      Some board
    with Not_found ->
      None

let possible_moves =
  let direction_of_pair (dcol, drow) = { dcol; drow } in
  let all_directions = List.map direction_of_pair [
      (1, 0); (-1, 0); (0, 1); (0, -1);
    ]
  in
  let all_moves : (piece * direction) list =
    List.(flatten (map (fun p -> map (fun d -> (p, d)) all_directions) all_pieces))
  in
  fun board -> List.((fold_left (fun s (p, dir) ->
      match move_piece board p dir with
      | Some board -> (Move (p, dir, board)) :: s
      | None -> s
    ) [] all_moves))

let klotski = { move; possible_moves; final }

module BoardSet = Set.Make (struct
    type t = board
    exception Result of int
    let compare =
      let index_of_piece = function
        | S -> 4
        | H -> 3
        | C -> 2
        | V -> 1
        | X -> 0
      in
      let compare_piece (i1, n1) (i2, n2) =
        let i = index_of_piece i1 - index_of_piece i2 in
        if i <> 0 then i else n1 - n2
      in
      fun bs1 bs2 ->
        try
	  for row = 0 to 4 do
            let r1 = bs1.(row) in
            let r2 = bs2.(row) in
	    for col = 0 to 3 do
              let c = compare_piece r1.(col) r2.(col) in
              if c <> 0 then raise (Result c);
            done;
	  done;
	  0
        with Result x -> x
  end)

let solve_klotski =
  let opset = {
    empty = BoardSet.empty;
    add = (fun x s -> BoardSet.add (List.hd x) s);
    mem = (fun c s -> BoardSet.mem (List.hd c) s)
  }
  in
  fun initial_board -> solve_puzzle klotski opset initial_board
