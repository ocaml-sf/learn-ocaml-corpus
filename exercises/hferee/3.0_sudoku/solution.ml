(* Question 1 *)
let p1 (x, _, _) = x
let p2 (_, y, _) = y
let p3 (_, _, z) = z
let app3 f (x, y, z) = (f x, f y, f z)

let all check (a, b, c) = check a && check b && check c
let exist test (x, y, z) = test x || test y || test z
let exist_pair test (x, y, z) = test x y || test x z || test y z

(* Question 2 *)
let within_bounds x = 1 <= x && x <= 9
let within_bounds_small_line = all within_bounds
let within_bounds_block : block -> bool = all within_bounds_small_line
let within_bounds_grid = all (all within_bounds_block)

(* Question 3 *)
let differents_small_line l = not (exist_pair (=) l)
let within t x = exist ((=) x) t
let intersects t1 = exist (within t1)
let differents_block b = all differents_small_line b &&
  not (exist_pair intersects b)

(* Question 4 *)
let block_correct c = within_bounds_block c && differents_block c

let blocks_correct : grid -> bool = all (all block_correct)

(* Question 6 *)

let transpose9 ll = app3 p1 ll, app3 p2 ll, app3 p3 ll

(* Question 7 *)
let transpose_lines_blocks : grid -> grid = app3 transpose9

let lines_correct (g : grid) : bool =
  blocks_correct (transpose_lines_blocks g)

(* Question 8 *)
let transpose_blocks : grid -> grid = app3 (app3 transpose9)

let transpose_grid (g : grid) : grid =
  transpose9 (transpose_blocks g)
let columns_correct (g : grid) =
  lines_correct (transpose_grid g)

let correct (g : grid) : bool =
  lines_correct g && columns_correct g && blocks_correct g

  (* support des grilles partielles (avec 0) et solveur *)

  (*
  module Partiel = struct
    let bornes x = 0 <= x && x <= 9
    let bornes_small_line = all bornes
    let bornes_bloc : block -> bool = all bornes_small_line

    let check_dup1 a (mem : int -> bool) : (int -> bool) option = if mem a && a <> 0 then None else Some (fun x -> x = a || mem x)
    let check_dup_pl ((a, b, c) : small_line) mem : (int -> bool) option =
      Option.bind (Option.bind (Option.bind (Some mem) (check_dup1 a)) (check_dup1 b)) (check_dup1 c)
      (* check if there is a non-zero duplicate in  a block*)
    let check_dup_bloc ((a, b, c) : block) : (int -> bool) option =
      Option.bind (Option.bind (Option.bind (Some (fun _ -> false)) (check_dup_pl a)) (check_dup_pl b)) (check_dup_pl c)


    (* Question 5 *)
    let block_correct c = bornes_bloc c && check_dup_bloc c <> None

    let trois_blocs_corrects = all block_correct

    let blocks_correct : grid -> bool = all trois_blocs_corrects

    (* Question 6 *)
    let p1 (x, _, _) = x
    let p2 (_, y, _) = y
    let p3 (_, _, z) = z


    let transpose9 ll = app3 p1 ll, app3 p2 ll, app3 p3 ll

    let transpose_lines_blocks : grid -> grid = app3 transpose9

    let lines_correct (g : grid) : bool =
      blocks_correct (transpose_lines_blocks g)

    (* Question 7 *)
    let transpose_blocks : grid -> grid = app3 (app3 transpose9)

    let transpose_grid (g : grid) : grid =
      transpose9 (transpose_blocks g)
    let columns_correct (g : grid) =
      lines_correct (transpose_grid g)

    let correct (g : grid) : bool =
      lines_correct g && columns_correct g && blocks_correct g

      let fill_case (v : int) (a : int) : int option = if a = 0 then Some v else None

    let fill_thing fill_down (v : int) (a, b, c) =
      match fill_down v a with
      | Some a' -> Some (a', b, c)
      | None -> match fill_down v b with
        | Some b' -> Some (a, b', c)
        | None -> match fill_down v c with
          | Some c' -> Some (a, b, c')
          | None -> None


      let fill_small_line = fill_thing fill_case
      let fill_bloc : int -> block -> block option = fill_thing fill_small_line
      let fill_gl = fill_thing fill_bloc
      let fill_grille : int -> grid -> grid option = fill_thing fill_gl

      let rec next_grille v g =
        if v > 9 then None
        else match fill_grille v g with
             | Some g' when correct g' -> Some (g', v)
             | _ -> next_grille (v + 1) g

      let complete : grid -> bool = all (all (all (all (( <> ) 0) )))
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
