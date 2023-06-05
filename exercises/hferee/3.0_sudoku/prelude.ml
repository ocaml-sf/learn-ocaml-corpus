
(* petite ligne de trois nombres *)
type petite_ligne = int * int * int

(* petite bloc de taille 3 x 3 *)
type bloc = petite_ligne * petite_ligne * petite_ligne

(* grande ligne de 3 x 9 *)
type grande_ligne = bloc * bloc * bloc

(* grille complète *)
type grille = grande_ligne * grande_ligne * grande_ligne

let make3 f = fun () -> (f(), f(), f())
let grille_aleatoire : unit -> grille =  (fun () -> Random.int 9 + 1) |> make3 |> make3 |> make3 |> make3

let print_grille f (g : grille) : unit =
  let _ = Format.pp_print_string f "\n" in
  let print_string = Format.pp_print_string f in
  let print_int n = print_string (string_of_int n) in
  let print3 f sep (a, b, c) = f a; print_string sep; f b; print_string sep; f c in
  let print_grille_aux (g : grille ) : unit =
    print3 (print3 (print3 (print3 print_int " ")
                            " | ")
                    "\n")
            "\n------+-------+------\n" g
    ; print_string "\n" in
  let a t (x, y, z) = t x, t y, t z in
  print_grille_aux (a (fun ll -> a (fun (x, _, _) -> x) ll, a (fun (_, x, _) -> x) ll, a (fun (_, _, x) -> x) ll) g);;


#install_printer print_grille;;
