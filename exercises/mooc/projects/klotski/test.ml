open Report
open Test_lib

let graded n cb =
  match !graded_selection with
  | None -> cb ()
  | Some graded ->
      if List.mem n graded then cb ()
      else Report.[ Message ([ Text "Skipped."], Important) ]

let int_set_operations =
  IntSet.{ empty ;
           add = printable_fun "add" add ;
           mem = printable_fun "mem" mem }
let sample_int_set_operations () =
  int_set_operations

let int_list_set_operations =
  IntListSet.{ empty ;
               add = printable_fun "add" add ;
               mem = printable_fun "mem" mem }
let sample_int_list_set_operations () =
  int_list_set_operations

let rec power_of n v =
  if v <= 0 then invalid_arg "power_of"
  else if v = 1 then true
  else if v mod n <> 0 then false
  else power_of n (v / n)

type int_prop = int prop
let sample_int_prop =
  sample_cases
    [ printable_fun "odd" (fun x -> x mod 2 = 1) ;
      printable_fun "even" (fun x -> x mod 2 = 0) ;
      printable_fun "(power_of 2)" (power_of 2) ;
      printable_fun "(power_of 3)" (power_of 3) ]

type int_int_function = int -> int
let sample_int_int_function =
  sample_cases
    [ printable_fun "succ" (fun x -> x + 1) ;
      printable_fun "pred" (fun x -> x - 1) ]

let sample_int () = Random.int 200 + 1

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1:" ; Code "loop" ],
           graded 1 @@ fun () ->
           test_function_3_against_solution
             [%ty: int_prop -> int_int_function -> int -> int] "loop"
             [])

let sample_int () = Random.int 16 + 1
let sample_list s () = Test_lib.sample_list ~min_size: 1 ~max_size: 4 ~dups: false s ()

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2:" ; Code "exists" ],
           graded 2 @@ fun () ->
           test_function_2_against_solution
             [%ty: int_prop -> int list -> bool] "exists"
             [ sample_int_prop (), [] ])

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3:" ; Code "find" ],
           graded 3 @@ fun () ->
           test_function_2_against_solution
             [%ty: int_prop -> int list -> int] "find"
             [ sample_int_prop (), [] ])

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4:" ; Code "near" ],
           graded 4 @@ fun () ->
           test_function_1_against_solution
             ~test: (test_canon_ok (List.sort_uniq compare))
             [%ty: int -> int list] "near"
             [])

type int_rel = int rel

let near = fun x -> [ x - 2 ; x - 1 ; x ; x + 1 ; x + 2 ]
let sample_int_rel =
  sample_cases
    [ printable_fun "near" near ;
      printable_fun "empty" (fun _ -> []) ;
      printable_fun "id" (fun x -> [ x ]) ]

let exercise_5 =
  set_progress "Grading exercise 5." ;
  Section ([ Text "Exercise 5:" ; Code "flat_map" ],
           graded 5 @@ fun () ->
           test_function_2_against_solution
             ~test: (test_canon_ok (List.sort_uniq compare))
             [%ty: int_rel -> int list -> int list] "flat_map"
             [])


type iter_n = int
let sample_iter_n () = Random.int 2 + 2

let exercise_6 =
  set_progress "Grading exercise 6." ;
  Section ([ Text "Exercise 6:" ; Code "iter_rel" ],
           graded 6 @@ fun () ->
           test_function_3_against_solution
             ~test: (test_canon_ok (List.sort_uniq compare))
             [%ty: int_rel -> iter_n -> int -> int list] "iter_rel"
             [ near, 2, 2 ])

let sample_int_prop =
  sample_cases
    [ printable_fun "odd" (fun x -> x mod 2 = 1) ;
      printable_fun "even" (fun x -> x mod 2 = 0) ;
      printable_fun "(power_of 2)" (power_of 2) ;
      printable_fun "(power_of 3)" (power_of 3) ]

let sample_int_rel =
  sample_cases
    [ printable_fun "succ" (fun x -> [x + 1]) ;
      printable_fun "pred" (fun x -> [x - 1]) ]

let sample_int () =
  Random.int 50 + 50

let exercise_7 =
  set_progress "Grading exercise 7." ;
  Section ([ Text "Exercise 7:" ; Code "solve" ],
           graded 7 @@ fun () ->
           test_function_3_against_solution
             [%ty: int_rel -> int_prop -> int -> int] "solve"
             [])

let exercise_8 =
  set_progress "Grading exercise 8." ;
  Section ([ Text "Exercise 8:" ; Code "solve_path" ],
           graded 8 @@ fun () ->
           test_function_3_against_solution
             [%ty: int_rel -> int_prop -> int -> int list] "solve_path"
             [])

let sample_int_rel =
  sample_cases
    [ printable_fun "near" near ;
      printable_fun "succ" (fun x -> [x + 1]) ;
      printable_fun "pred" (fun x -> [x - 1]) ;
      printable_fun "id" (fun x -> [ x ]) ]

let exercise_9 =
  set_progress "Grading exercise 9." ;
  Section ([ Text "Exercise 9:" ; Code "archive_map" ],
           graded 9 @@ fun () ->
           test_function_3_against_solution
             ~sampler:
               (let int () = Random.int 20 - 10 in
                let list () = Test_lib.sample_list ~min_size: 5 ~max_size: 20 ~dups: false int () in
                let rec sublist = function
                  | [] -> []
                  | _ :: r when Random.bool () -> sublist r
                  | e :: r -> e :: sublist r in
                fun () ->
                  (int_set_operations,
                   sample_int_rel (),
                   (let l = list () in IntSet.of_list l, sublist l)))
             ~test: (test_eq_ok (fun (s1, l1) (s2, l2) ->
                 IntSet.equal s1 s2
                 && List.sort_uniq compare l1 = List.sort_uniq compare l2))
             [%ty: (int_set_operations ->
                    int_rel ->
                    (IntSet.t * int list) ->
                    (IntSet.t * int list))]
             "archive_map"
             [])

let sample_int_rel =
  sample_cases
    [ printable_fun "succ" (fun x -> [x + 1]) ;
      printable_fun "pred" (fun x -> [x - 1]) ]

let exercise_10 =
  set_progress "Grading exercise 10." ;
  Section ([ Text "Exercise 10:" ; Code "solve'" ],
           graded 10 @@ fun () ->
           test_function_4_against_solution
             [%ty: int_set_operations -> int_rel -> int_prop -> int -> int] "solve'"
             [])

let exercise_11 =
  set_progress "Grading exercise 11." ;
  Section ([ Text "Exercise 11:" ; Code "solve_path'" ],
           graded 11 @@ fun () ->
           if snd @@ Report.result @@
             test_variable_property
               [%ty: int_list_set_operations -> int_rel -> int_prop -> int -> int list]
               "solve_path'" (fun _ -> []) then
             test_function_4_against_solution
               [%ty: int_set_operations -> int_rel -> int_prop -> int -> int list] "solve_path'"
               []
           else
             test_function_4_against_solution
               [%ty: int_list_set_operations -> int_rel -> int_prop -> int -> int list] "solve_path'"
               [])

type rabbit_move = Forward | Backward | Jump
type rabbit_game = (int, rabbit_move) puzzle
let rabbit_moves _ =  [ Forward ; Backward ; Jump ]
let move_rabbit pos = function
  | Forward -> pos + 1
  | Backward -> pos - 3
  | Jump -> pos * 2

let sample_rabbit_game () =
  let pos = Random.int 23 + 10 in
  { move = printable_fun "move_rabbit" move_rabbit ;
    possible_moves = printable_fun "rabbit_moves" rabbit_moves ;
    final = printable_fun (Printf.sprintf "((=) %d)" pos) ((=) pos) }

let sample_int () = Random.int 10 + 10

let exercise_12 =
  set_progress "Grading exercise 12." ;
  Section ([ Text "Exercise 12:" ; Code "solve_puzzle" ],
           graded 12 @@ fun () ->
           [ Message ([
                 Text "Now I will test your code on a little game." ; Break ;
                 Text "It is called \"Mister Rabbit's Great Escape\" ." ; Break ;
                 Text "Mister rabbit is pursued by a dog, and has \
                       to find his way home." ; Break ;
                 Text "He just has to run straight from point (A) to point (B)." ; Break ;
                 Text "In our case, these will just be integers." ; Break ;
                 Text "But mister Rabbit is easily scared." ; Break ;
                 Text "So he walks very cautiously:" ; Code "(fun x -> pos + 1)." ; Break ;
                 Text "When he is surprised, he runs backward:" ; Code "(fun x -> pos - 3)." ; Break ;
                 Text "And on a rush, he can jump very far:" ; Code "(fun x -> pos * 2)." ], Informative) ;
             Message ([
                 Text "To sum up, your solver will be tested on the following:" ; Break ;
                 Code "type rabbit_move = Forward | Backward | Jump\n\
                       let rabbit_moves _ =  [ Forward ; Backward ; Jump ]\n\
                       let move_rabbit pos = function\n\
                      \  | Forward -> pos + 1\n\
                      \  | Backward -> pos - 3\n\
                      \  | Jump -> pos * 2" ], Important) ] @
           test_function_3_against_solution
             ~test: (test_eq_ok
                       (fun got exp ->
                          let rec eq = function
                            | [ got ], [ exp ] -> got = exp
                            | g :: (g' :: _ as gs), _ :: es ->
                               (g' = g + 1 || g' = g - 3 || g' = 2 * g) && eq (gs, es)
                            | _, _ -> false in
                          List.hd got = List.hd exp
                          && eq (got, exp)))
             [%ty: rabbit_game -> int_list_set_operations -> int -> int list] "solve_puzzle"
             [])

let find_spot arr w h =
  let res, nb = ref [], ref 0 in
  for x = 0 to 4 - w do
    for y = 0 to 5 - h do
      let r = ref true in
      for i = x to x + w - 1 do
        for j = y to y + h - 1 do
          r := !r && arr.(j).(i) = (X, 0)
        done
      done ;
      if !r then begin
        res := (x, y) :: !res ;
        incr nb
      end
    done
  done ;
  if !nb = 0 then raise Not_found
  else List.nth !res (Random.int !nb)

let last_board = ref [||]

let sample_board =
  let sample win () =
    let arr = Array.make_matrix 5 4 (X, 0) in
    let put v x y w h =
      for i = x to x + w - 1 do
        for j = y to y + h - 1 do
          arr.(j).(i) <- v
        done
      done in
    if win then begin
      put (S, 0) 1 3 2 2
    end else begin
      put (S, 0) (Random.int 2) (Random.int 3) 2 2
    end ;
    List.iter
      (fun (p, w, h) -> try
          let x, y = find_spot arr w h in
          put p x y w h
        with Not_found -> ())
      [ (H, 0), 2, 1 ;
        (V, 0), 1, 2 ;
        (V, 1), 1, 2 ;
        (V, 2), 1, 2 ;
        (V, 3), 1, 2 ;
        (C, 0), 1, 1 ;
        (C, 1), 1, 1 ;
        (C, 2), 1, 1 ;
        (C, 3), 1, 1 ] ;
    last_board := arr ;
    arr in
  sample_alternatively [ sample true ; sample false ]

let exercise_13 =
  set_progress "Grading exercise 13." ;
  Section ([ Text "Exercise 13:" ; Code "final" ],
           graded 13 @@ fun () ->
           test_function_1_against_solution
             [%ty: board -> bool] "final"
             [])

let find_possible_move board =
  let x, y = find_spot board 1 1 in
  let get y x =
    if x < 0 || y < 0 || x > 3 || y > 4 then (X, 999)
    else board.(y).(x) in
  match
    [| [| get (y-1) (x-1) ; get (y-1) x ; get (y-1) (x+1) |] ;
       [| get   y   (x-1) ; get   y   x ; get   y   (x+1) |] ;
       [| get (y+1) (x-1) ; get (y+1) x ; get (y+1) (x+1) |] |]
  with
  | [| [| (S,0) ; (X,0) ; (_,_) |] ;
       [| (S,0) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (S,0) ; (X,0) ; (_,_) |] ;
       [| (S,0) ; (X,0) ; (_,_) |] |] ->
      board.(y).(x - 1), { dcol = 1 ; drow = 0 }
  | [| [| (V,a) ; (X,0) ; (_,_) |] ;
       [| (V,b) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (V,a) ; (X,0) ; (_,_) |] ;
       [| (V,b) ; (X,0) ; (_,_) |] |] when a = b ->
      board.(y).(x - 1), { dcol = 1 ; drow = 0 }
  | [| [| (_,_) ; (X,0) ; (S,0) |] ;
       [| (_,_) ; (X,0) ; (S,0) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (S,0) |] ;
       [| (_,_) ; (X,0) ; (S,0) |] |] ->
      board.(y).(x + 1), { dcol = -1 ; drow = 0 }
  | [| [| (_,_) ; (X,0) ; (V,a) |] ;
       [| (_,_) ; (X,0) ; (V,b) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (V,a) |] ;
       [| (_,_) ; (X,0) ; (V,b) |] |] when a = b ->
      board.(y).(x + 1), { dcol = -1 ; drow = 0 }
  | [| [| (H,0) ; (H,0) ; (_,_) |] ;
       [| (X,0) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (S,0) ; (S,0) ; (_,_) |] ;
       [| (X,0) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (H,0) ; (H,0) |] ;
       [| (_,_) ; (X,0) ; (X,0) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (S,0) ; (S,0) |] ;
       [| (_,_) ; (X,0) ; (X,0) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |] ->
      board.(y - 1).(x), { dcol = 0 ; drow = 1 }
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (X,0) ; (X,0) ; (_,_) |] ;
       [| (H,0) ; (H,0) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (X,0) ; (X,0) ; (_,_) |] ;
       [| (S,0) ; (S,0) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (X,0) |] ;
       [| (_,_) ; (H,0) ; (H,0) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (X,0) |] ;
       [| (_,_) ; (S,0) ; (S,0) |] |] ->
      board.(y + 1).(x), { dcol = 0 ; drow = -1 }
  | [| [| (_,_) ; (V,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (C,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |] ->
      board.(y - 1).(x), { dcol = 0 ; drow = 1 }
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (V,_) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (C,_) ; (_,_) |] |] ->
      board.(y + 1).(x), { dcol = 0 ; drow = -1 }
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (H,0) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (C,_) ; (X,0) ; (_,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |] ->
      board.(y).(x - 1), { dcol = 1 ; drow = 0 }
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (H,0) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |]
  | [| [| (_,_) ; (_,_) ; (_,_) |] ;
       [| (_,_) ; (X,0) ; (C,_) |] ;
       [| (_,_) ; (_,_) ; (_,_) |] |] ->
      board.(y).(x + 1), { dcol = -1 ; drow = 0 }
  | _ -> raise Not_found

let find_impossible_move board =
  let rec find board retries =
    if retries = 0 then
      raise Not_found
    else
      let x, y = Random.int 4, Random.int 5 in
      match board.(y).(x) with
      | (X, _) -> find board (retries - 1)
      | p ->
          let dir = match Random.int 4 with
            | 0 -> { dcol = 0 ; drow = 1 }
            | 1 -> { dcol = 1 ; drow = 0 }
            | 2 -> { dcol = -1 ; drow = 0 }
            | _ -> { dcol = 0 ; drow = -1 } in
          match Solution.move_piece board p dir with
          | None -> p, dir
          | Some _ -> find board (retries - 1) in
  find board 10

let exercise_14 =
  set_progress "Grading exercise 14." ;
  Section ([ Text "Exercise 14:" ; Code "move_piece" ],
           graded 14 @@ fun () ->
           test_function_3_against_solution
             ~sampler:
               (let sampler find_move () =
                  let board = sample_board () in
                  try
                    let piece, direction = find_move board in
                    board, piece, direction
                  with Not_found -> board, (S, 0), { dcol = 0 ; drow = 1 } in
                sample_alternatively
                  [ sampler find_possible_move ;
                    sampler find_impossible_move ])
             [%ty: board -> piece -> direction -> board option] "move_piece"
             [])

let exercise_15 =
  set_progress "Grading exercise 15." ;
  Section ([ Text "Exercise 15:" ; Code "possible_moves" ],
           graded 15 @@ fun () ->
           test_function_1_against_solution
             ~test: (test_canon_ok (List.sort compare))
             [%ty: board -> move list] "possible_moves"
             [])

module type BoardSet_S = Set.S with type elt = board

let sampler =
  let neq_by_one_move () =
    let board = sample_board () in
    board, try
      let piece, direction = find_possible_move board in
      match Solution.move_piece board piece direction with
      | None -> raise Not_found
      | Some board' -> board'
    with _ -> board in
  let eq () =
    let board = sample_board () in
    board, board in
  let neq () =
    sample_board (), sample_board ()  in
  sample_alternatively
    [ eq ; neq ; neq_by_one_move ; neq_by_one_move ]

let exercise_16 =
  set_progress "Grading exercise 16" ;
  Section ([ Text "Exercise 16:" ; Code "BoardSet" ],
           graded 16 @@ fun () ->
           test_variable_property
             [%ty: (module BoardSet_S)]
             "BoardSet" @@ fun (module BoardSet : BoardSet_S) ->
           [ Message ([ Text "Your module has the expected signature. Well done!" ], Success 5) ;
             Message ([ Text "Now, I will check your comparison." ], Important) ] @
           test_function_2_against_solution ~sampler ~gen:15
             ~test: (test_canon_ok (fun x -> if x = 0 then 0 else if x < 0 then -1 else 1))
             [%ty: board -> board -> int] "BoardSet.compare_elt"
             [])

let exercise_17 =
  set_progress "Grading exercise 17" ;
  Section ([ Text "Exercise 17:" ; Code "compare" ],
           graded 17 @@ fun () ->
           if snd (Report.result [ exercise_16 ]) then
             [ Message ([ Text "Please complete the previous exercise first." ], Failure) ]
           else
             let got = ref 0 and exp = ref 0 in
             test_function_2_against_solution ~sampler ~gen:15
               ~before_reference: (fun _ _ ->
                   array_get_counter := 0)
               ~before_user: (fun _ _ ->
                   exp := !array_get_counter ;
                   array_get_counter := 0)
               ~after: (fun _ _ _ _ ->
                   got := !array_get_counter ;
                   [ Message ([ Text "Checking the number of array accesses." ], Informative) ] @
                   test [%ty: int] (Ok !got) (Ok !exp))
               ~test: (test_canon_ok (fun x -> if x = 0 then 0 else if x < 0 then -1 else 1))
               [%ty: board -> board -> int] "BoardSet.compare_elt"
               [])

let rec valid_solution board = function
  | [] -> Solution.final board
  | next :: rest ->
      List.exists
        (fun (Move (_, _, next')) -> next = next')
        (Solution.possible_moves board)
      && valid_solution next rest

let exercise_18 =
  set_progress "Grading exercise 18" ;
  Section ([ Text "Exercise 18:" ; Code "solve_klotski" ],
           graded 18 @@ fun () ->
           test_variable_property
             [%ty: board -> board list] "solve_klotski" @@ fun solve ->
           [ Message ([ Text "Computing" ; Code "solve initial_board_simpler" ], Informative) ] @
           match result (fun () -> solve initial_board_simpler) with
           | Error exn ->
               [ Message ([ Text "Wrong exception" ; Code (Printexc.to_string exn) ], Failure) ]
           | Ok sol when valid_solution initial_board_simpler sol ->
               [ Message ([ Text "Your solver is correct. Congratulations !" ], Success 10) ]
           | Ok _ ->
               [ Message ([ Text "Your solver returned an incorrect solution." ], Failure) ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  begin match !graded_selection with
    | None -> []
    | Some _ -> [ Message ([ Text "Some exercises have been skipped." ], Failure) ]
  end @
  [ Section ([ Text "Preliminaries" ],
             [ exercise_1 ; exercise_2 ; exercise_3 ]) ;
    Section ([ Text "Part A: A Generic Problem Solver" ],
             [ exercise_4 ; exercise_5 ; exercise_6 ;
               exercise_7 ; exercise_8 ; exercise_9 ;
               exercise_10 ; exercise_11 ; exercise_12 ]) ;
    Section ([ Text "Part B: A Solver for Klotski" ],
             [ exercise_13 ; exercise_14 ; exercise_15 ;
               exercise_16 ; exercise_17 ; exercise_18 ]) ]
