
(* small line of three numbers *)
type small_line = int * int * int

(* small size block 3 x 3 *)
type block = small_line * small_line * small_line

(* large 3 x 9 line *)
type big_line = block * block * block

(* full grid *)
type grid = big_line * big_line * big_line

let make3 f = fun () -> (f(), f(), f())
let random_grid : unit -> grid =  (fun () -> Random.int 9 + 1) |> make3 |> make3 |> make3 |> make3

let print_grid f (g : grid) : unit =
  let _ = Format.pp_print_string f "\n" in
  let print_string = Format.pp_print_string f in
  let print_int n = print_string (string_of_int n) in
  let print3 f sep (a, b, c) = f a; print_string sep; f b; print_string sep; f c in
  let print_grid_aux (g : grid ) : unit =
    print3 (print3 (print3 (print3 print_int " ")
                            " | ")
                    "\n")
            "\n------+-------+------\n" g
    ; print_string "\n" in
  let a t (x, y, z) = t x, t y, t z in
  print_grid_aux (a (fun ll -> a (fun (x, _, _) -> x) ll, a (fun (_, x, _) -> x) ll, a (fun (_, _, x) -> x) ll) g);;
