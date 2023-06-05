(* let movimento_possivel x y game_matrix =
       let n = Array.length game_matrix in
       if x > (n-1) || y > (n-1) || x < 0 || y < 0 then false
       else true

   let rec zombie_attack game_matrix =
     let trocas = ref (false) in
     let n = Array.length game_matrix in
     begin
       for linhas = 0 to (n-1) do
         for colunas = 0 to (n-1) do
           match game_matrix.(linhas).(colunas) with
           |'*' -> if movimento_possivel (linhas-1) (colunas) game_matrix && game_matrix.(linhas-1).(colunas) = '.'  then (Printf.printf "Bruh momentum ";game_matrix.(linhas-1).(colunas) <- '*'; trocas := true);
                   if movimento_possivel (linhas+1) (colunas) game_matrix && game_matrix.(linhas+1).(colunas) = '.'  then (game_matrix.(linhas+1).(colunas) <- '*'; trocas := true);
                   if movimento_possivel (linhas) (colunas-1) game_matrix && game_matrix.(linhas).(colunas-1) = '.'  then (game_matrix.(linhas).(colunas-1) <- '*'; trocas := true);
                   if movimento_possivel (linhas) (colunas+1) game_matrix && game_matrix.(linhas).(colunas+1) = '.'  then (game_matrix.(linhas).(colunas+1) <- '*'; trocas := true);
           |_ -> ()
         done
       done;
       if !trocas then zombie_attack game_matrix else game_matrix
     end *)

(* Another possible solution *)
let rec zombie_attack (game_matrix : char array array) : char array array =
  let rec zombie_attack_aux (game_matrix : char array array) (i : int) (j : int)
      : unit =
    if
      i < 0
      || i >= Array.length game_matrix
      || j < 0
      || j >= Array.length game_matrix.(0)
    then ()
    else if game_matrix.(i).(j) = '*' then ()
    else if game_matrix.(i).(j) = 'x' then game_matrix.(i).(j) <- '*'
    else (
      game_matrix.(i).(j) <- '*';
      zombie_attack_aux game_matrix (i + 1) j;
      zombie_attack_aux game_matrix (i - 1) j;
      zombie_attack_aux game_matrix i (j + 1);
      zombie_attack_aux game_matrix i (j - 1))
  in
  for i = 0 to Array.length game_matrix - 1 do
    for j = 0 to Array.length game_matrix.(0) - 1 do
      if game_matrix.(i).(j) = 'x' then zombie_attack_aux game_matrix i j
    done
  done;
  game_matrix