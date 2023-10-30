open Test_lib
open Report

let gen_sample_matrix () =
  let () = Random.self_init () in
  let n = Random.int 9 + 7 in
  let array_test = Array.make_matrix n n '.' in
  let zombie_number = if n > 12 then 1 else 2 in
  let cat_number = if n > 12 then 7 else 10 in
  for i = 0 to zombie_number do
    let x = Random.int (n - 1) and y = Random.int (n - 1) in
    if array_test.(x).(y) = '.' then array_test.(x).(y) <- '*'
    else
      let flag = ref true in
      while !flag do
        let x2 = Random.int (n - 1) and y2 = Random.int (n - 1) in
        if array_test.(x2).(y2) = '.' then (
          array_test.(x).(y) <- '*';
          flag := false)
      done
  done;
  for i = 0 to cat_number do
    let x = Random.int (n - 1) and y = Random.int (n - 1) in
    if array_test.(x).(y) = '.' then array_test.(x).(y) <- 'X'
    else
      let flag = ref false in
      while !flag do
        let x2 = Random.int (n - 1) and y2 = Random.int (n - 1) in
        if array_test.(x2).(y2) = '.' then (
          array_test.(x).(y) <- 'X';
          flag := false)
      done
  done;
  array_test

let zombie_attackK =
  set_progress "Grading exercise";
  Section
    ( [ Text "Testing function"; Code "zombie_attack" ],
      test_function_1_against_solution
        [%ty: char array array -> char array array] "zombie_attack"
        ~sampler:gen_sample_matrix ~gen:10
        [
          [|
            [| '*'; '.'; '.'; '.'; '*'; '.'; '.' |];
            [| '.'; '.'; 'X'; 'X'; '.'; '.'; '.' |];
            [| '.'; 'X'; '.'; '.'; 'X'; '.'; '.' |];
            [| '.'; '.'; 'X'; '.'; '.'; 'X'; '.' |];
            [| 'X'; '.'; 'X'; '.'; 'X'; '.'; '.' |];
            [| '.'; 'X'; '.'; 'X'; '.'; '.'; '.' |];
            [| 'X'; '.'; '.'; '.'; '.'; '.'; '*' |];
          |];
        ] )

let () = set_result @@ ast_sanity_check code_ast @@ fun () -> [ zombie_attackK ]