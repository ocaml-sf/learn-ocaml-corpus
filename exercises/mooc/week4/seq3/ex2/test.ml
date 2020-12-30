open Report
open Test_lib

type func = int -> int -> int

let funcs =
  [| "add", printable_fun "add" (+);
     "sub", printable_fun "sub" (-);
     "mul", printable_fun "mul" ( * );
     "div", printable_fun "div" ( / );
     "mod", printable_fun "mod" ( mod );
     "and", printable_fun "and" ( land );
     "or", printable_fun "or" ( lor );
     "xor", printable_fun "xor" ( lxor ); |]

let sample_inside () =
  Array.sort (fun _ _ -> Random.int 2 * 2 - 1) funcs ;
  let env = Array.sub funcs 0 (Random.int (Array.length funcs - 1) + 1) in
  env.(Random.int (Array.length env)), Array.to_list env

let sample_outside () =
  Array.sort (fun _ _ -> Random.int 2 * 2 - 1) funcs ;
  let arr = Array.sub funcs 0 (Random.int (Array.length funcs - 1) + 1) in
  match Array.to_list arr with
  | [] -> assert false
  | f :: env -> f, env

let exercise_1 =
  let open Parsetree in
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "lookup_function" ],
           find_binding code_ast "lookup_function" @@ fun expr ->
           ast_check_expr
             ~on_expression:(function
                   [%expr List.assoc] | [%expr assoc] ->
                     [ failure
                         ~message:"You cannot use the List.assoc function." ]
                 | _ -> []) expr @
           test_function_2_against_solution
             ~test:(test_eq_ok (==))
             ~sampler:
               (let sample = sample_alternatively [ sample_inside ; sample_outside ] in
                fun () -> let ((n, _), env) = sample () in (n, env))
             [%ty: string -> env -> func] "lookup_function"
             [])


let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "add_function" ],
           test_function_3_against_solution
             ~test:(test_eq_ok @@ fun env1 env2 ->
                    let tr env =
                      let env = List.map (fun (n, _) -> n) env in
                      List.sort compare env in
                    tr env1 = tr env2)
             ~sampler:
               (fun () -> let ((n, f), env) = sample_outside () in (n, f, env))
             [%ty: string -> func -> env -> env]
             "add_function"
             [])

let check_min l =
  let rec compute_min min acc i =
    if i <= 0 then acc else
      let x = Random.int 50 - 25 and y = Random.int 50 - 25 in
      let v1 = result @@ fun () -> min x y in
      let v2 = result @@ fun () -> Pervasives.min x y in
      compute_min min
        (test [%ty: int ] v1 v2 @
         [ Message ([ Text "Computing " ; Code (Format.asprintf "min %d %d" x y) ], Informative) ] @
         acc) (i-1) in
  let check_present n l =
    if List.mem_assoc n l then [] else
      [ Message ([ Text "Missing function" ; Code n ; Text "from the initial environment." ], Failure) ]  in
  (List.map (fun (n, _) -> check_present n l) initial_env |> List.flatten) @
  try
    let min = List.assoc "min" l in
    [ Message ([ Text "The min function has correctly been added to" ; Code "my_env" ], Informative) ] @
    (compute_min min [] 5 |> List.rev)
  with Not_found ->
    [failure ~message:"I cannot find 'min' in 'my_env'."]

let exercise_3 =
  let open Parsetree in
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "my_env" ],
           find_binding code_ast "my_env" @@ fun expr ->
           ast_check_expr
             ~on_expression:(function
                   [%expr Pervasives.min] | [%expr min] ->
                     [ failure
                         ~message:"You cannot use the Pervasives.min function." ]
                 | { pexp_desc = Pexp_let (_, _, _) } ->
                     [ failure
                       ~message:"You cannot use any let expression to define \
                                 'my_env'" ]
                 | _ -> []) expr @
           test_variable_property
             [%ty: (string * (int -> int -> int)) list] "my_env" check_min)

let funcs =
  Array.sort (fun _ _ -> Random.int 2 * 2 - 1) funcs ;
  Array.sub funcs 0 3

let sample_env () =
  Array.to_list funcs

let sample_funcname =
  List.split (Array.to_list funcs) |> fst |> sample_cases

let sample_operation () =
  let node_nb = Random.int 10 in
  let rec gen_node nb =
    if nb > 0 then
      let dir = Random.int 9 in
      if dir < 3 then
        let left, rem = gen_node (nb-1) in
        let right, rem = gen_node rem in
        Op (sample_funcname (), left, right), rem
      else if dir < 6 then
        let right, rem = gen_node (nb-1) in
        let left, rem = gen_node rem in
        Op (sample_funcname (), left, right), rem
      else
        Value (sample_int ()), nb
    else
      Value (sample_int ()), nb
  in
  gen_node node_nb |> fst

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "compute" ],
           test_function_2_against_solution
             [%ty: env -> operation -> int] "compute"
             [])

let exercise_5 =
  let open Parsetree in
  set_progress "Grading exercise 5." ;
  Section ([ Text "Exercise 5: " ; Code "compute_eff" ],
           find_binding code_ast "compute_eff" @@ fun expr ->
           ast_check_expr
             ~on_expression:(function
                   [%expr compute] ->
                     [ failure
                         ~message:"You cannot reuse the compute function." ]
                 | { pexp_desc = Pexp_let (_, _, _) } ->
                     [ failure
                       ~message:"You cannot use any let expression to define \
                                 'compute_eff'" ]
                 | _ -> []) expr @
           test_function_2_against_solution
             [%ty: env -> operation -> int] "compute_eff"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
    [ exercise_1 ;
      exercise_2 ;
      exercise_3 ;
      exercise_4 ;
      exercise_5
    ]
