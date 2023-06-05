(* Question 1 *)
let p1 (x, _, _) = x
let p2 (_, y, _) = y
let p3 (_, _, z) = z
let app3 f (x, y, z) = (f x, f y, f z)

let tous check (a, b, c) = check a && check b && check c
let existe test (x, y, z) = test x || test y || test z
let existe_paire test (x, y, z) = test x y || test x z || test y z

(* Question 2 *)
let dans_bornes x = 1 <= x && x <= 9
let dans_bornes_petite_ligne = tous dans_bornes
let dans_bornes_bloc : bloc -> bool = tous dans_bornes_petite_ligne
let dans_bornes_grille = tous (tous dans_bornes_bloc)

(* Question 3 *)
let differents_petite_ligne l = not (existe_paire (=) l)
let dans t x = existe ((=) x) t
let intersecte t1 = existe (dans t1)
let differents_bloc b = tous differents_petite_ligne b &&
  not (existe_paire intersecte b)

(* Question 4 *)
let bloc_correct c = dans_bornes_bloc c && differents_bloc c

let blocs_corrects : grille -> bool = tous (tous bloc_correct)

(* Question 6 *)

let transpose9 ll = app3 p1 ll, app3 p2 ll, app3 p3 ll

(* Question 7 *)
let transpose_lignes_blocs : grille -> grille = app3 transpose9

let lignes_correctes (g : grille) : bool =
  blocs_corrects (transpose_lignes_blocs g)

(* Question 8 *)
let transpose_blocs : grille -> grille = app3 (app3 transpose9)

let transpose_grille (g : grille) : grille =
  transpose9 (transpose_blocs g)
let colonnes_correctes (g : grille) =
  lignes_correctes (transpose_grille g)

let correcte (g : grille) : bool =
  lignes_correctes g && colonnes_correctes g && blocs_corrects g

  (* support des grilles partielles (avec 0) et solveur *)

  (*
  module Partiel = struct
    let bornes x = 0 <= x && x <= 9
    let bornes_petite_ligne = tous bornes
    let bornes_bloc : bloc -> bool = tous bornes_petite_ligne

    let check_dup1 a (mem : int -> bool) : (int -> bool) option = if mem a && a <> 0 then None else Some (fun x -> x = a || mem x)
    let check_dup_pl ((a, b, c) : petite_ligne) mem : (int -> bool) option =
      Option.bind (Option.bind (Option.bind (Some mem) (check_dup1 a)) (check_dup1 b)) (check_dup1 c)
      (* check if there is a non-zero duplicate in  a bloc*)
    let check_dup_bloc ((a, b, c) : bloc) : (int -> bool) option =
      Option.bind (Option.bind (Option.bind (Some (fun _ -> false)) (check_dup_pl a)) (check_dup_pl b)) (check_dup_pl c)


    (* Question 5 *)
    let bloc_correct c = bornes_bloc c && check_dup_bloc c <> None

    let trois_blocs_corrects = tous bloc_correct

    let blocs_corrects : grille -> bool = tous trois_blocs_corrects

    (* Question 6 *)
    let p1 (x, _, _) = x
    let p2 (_, y, _) = y
    let p3 (_, _, z) = z


    let transpose9 ll = app3 p1 ll, app3 p2 ll, app3 p3 ll

    let transpose_lignes_blocs : grille -> grille = app3 transpose9

    let lignes_correctes (g : grille) : bool =
      blocs_corrects (transpose_lignes_blocs g)

    (* Question 7 *)
    let transpose_blocs : grille -> grille = app3 (app3 transpose9)

    let transpose_grille (g : grille) : grille =
      transpose9 (transpose_blocs g)
    let colonnes_correctes (g : grille) =
      lignes_correctes (transpose_grille g)

    let correcte (g : grille) : bool =
      lignes_correctes g && colonnes_correctes g && blocs_corrects g

      let fill_case (v : int) (a : int) : int option = if a = 0 then Some v else None

    let fill_thing fill_down (v : int) (a, b, c) =
      match fill_down v a with
      | Some a' -> Some (a', b, c)
      | None -> match fill_down v b with
        | Some b' -> Some (a, b', c)
        | None -> match fill_down v c with
          | Some c' -> Some (a, b, c')
          | None -> None


      let fill_petite_ligne = fill_thing fill_case
      let fill_bloc : int -> bloc -> bloc option = fill_thing fill_petite_ligne
      let fill_gl = fill_thing fill_bloc
      let fill_grille : int -> grille -> grille option = fill_thing fill_gl

      let rec next_grille v g =
        if v > 9 then None
        else match fill_grille v g with
             | Some g' when correcte g' -> Some (g', v)
             | _ -> next_grille (v + 1) g

      let complete : grille -> bool = tous (tous (tous (tous (( <> ) 0) )))
      let build_grille =
        let rec build_grille_aux min g =
          if min > 9 then None else
          if complete g then Some g
          else match next_grille min g with
            | Some (g', v) -> (match build_grille_aux 1 g' with
              | None -> build_grille_aux (min + 1) g
              | Some g'' -> Some g'')
            | None -> None
        in build_grille_aux 1
    (* tries to fill the next 0 *)
    end

*)
