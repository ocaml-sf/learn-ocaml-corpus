open Report
open Test_lib

type dir = string

let sample_dir =
  sample_cases [ "opt" ; "local" ; "My Documents" ; "Users" ]

type file = string

let sample_file =
  let sample_base =
    sample_cases [ "passwd" ; "config" ; "htaccess" ; "id_rsa" ; "funny" ; "report" ] in
  let sample_ext =
    sample_cases [ "doc" ; "ppt" ; "txt" ; "wav" ; "html" ] in
  fun () ->
    (if Random.int 5 = 0 then "." else "") ^ sample_base () ^ "." ^ sample_ext ()

let sample_int () =
  Random.int 5

type absolute_path = string list

let sample_absolute_path () =
  let rec sample level =
    match Random.int level with
    | 0 -> [ sample_file () ]
    | _ -> sample_dir () :: sample (level - 1) in
  sample 8

type relative_path = string list

let sample_relative_path () =
  let rec sample level =
    match Random.int level with
    | 0 -> [ sample_file () ]
    | _ ->
        (match Random.int 5 with
         | 1 | 2 -> ".."
         | _ -> sample_dir ()) :: sample (level - 1) in
  sample 8


let rec choose_file l acc =
  let len = List.length l in
  match List.nth l (Random.int len) with
  | name, File -> List.rev (name :: acc)
  | _, Symlink _ -> choose_file l acc
  | name, Dir fs -> choose_file fs (name :: acc)

let make_relative sym file =
  let rec eat_prefix = function
    | s :: sym, f :: file when s = f -> eat_prefix (sym, file)
    | sym, file -> sym, file in
  let rec up file = function
    | [] -> assert false
    | [ _ ] -> file
    | s :: sym -> ".." :: up file sym in
  let sym, file = eat_prefix (sym, file) in
  up file sym

let sample_filesystem () =
  let root =
    let twidth = 4 in
    let theight = 4 in
    let uniq = List.sort_uniq (fun (x, _) (y, _) -> compare x y) in
    let rec sample height width =
      let sample_item () =
        match Random.int height with
        | 0 -> [ sample_file (), File ]
        | 1 -> [ sample_file (), File ; sample_file (), Symlink (sample_relative_path ()) ]
        | _ -> [ sample_dir (), Dir (sample (height - 1) twidth |> uniq) ] in
      match Random.int width with
      | 0 -> sample_item ()
      | _ -> sample_item () @ sample height (width - 1) in
    sample theight twidth |> uniq in
  let rec fix_links acc = function
    | [] -> []
    | (name, File) :: rest ->
        (name, File) :: fix_links acc rest
    | (name, Dir fs) :: rest ->
        (name, Dir (fix_links (name :: acc) fs)) :: fix_links acc rest
    | (name, Symlink _) :: rest ->
        let path = make_relative (List.rev (name :: acc)) (choose_file root []) in
        (name, Symlink path) :: fix_links acc rest in
  fix_links [] root

let test_line = io_test_equals ~drop: [ ' ' ]
let test_stdout = io_test_lines ~trim: [ ' ' ] ~skip_empty: true ~test_line
let test = test_ignore

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "print_path" ],
           test_function_1_against_solution
             ~test_stdout ~test
             [%ty: relative_path -> unit] "print_path"
             [])

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "print_file" ],
           test_function_2_against_solution
             ~test_stdout ~test
             [%ty: int -> file -> unit] "print_file"
             [])

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "print_symlink" ],
           test_function_3_against_solution
             ~test_stdout ~test
             [%ty: int -> file -> relative_path -> unit] "print_symlink"
             [])

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "print_dir" ],
           test_function_2_against_solution
             ~test_stdout ~test
             [%ty: int -> dir -> unit] "print_dir"
             [])

let exercise_5 =
  set_progress "Grading exercise 5." ;
  Section ([ Text "Exercise 5: " ; Code "print_filesystem" ],
           test_function_1_against_solution
             ~test_stdout ~test
             [%ty: filesystem -> unit] "print_filesystem"
             [])

let exercise_6 =
  set_progress "Grading exercise 6." ;
  Section ([ Text "Exercise 6: " ; Code "resolve" ],
           test_function_2_against_solution
             [%ty: absolute_path -> relative_path -> absolute_path] "resolve"
             [])

let exercise_7 =
  set_progress "Grading exercise 7." ;
  Section ([ Text "Exercise 7: " ; Code "file_exists" ],
           test_function_2_against_solution
             ~sampler:(fun () ->
                 let fs = sample_filesystem () in
                 if Random.bool () then
                   fs, choose_file fs []
                 else
                   fs, sample_absolute_path ())
             [%ty: filesystem -> absolute_path -> bool] "file_exists"
             [])

let sample_filesystem () =
  let rec fuzz root =
    List.map (function
        | n, Symlink p when Random.bool () ->
            n, Symlink (sample_relative_path ())
        | n, Dir fs -> n, Dir (fuzz fs)
        | item -> item)
      root in
  fuzz (sample_filesystem ())

let exercise_8 =
  set_progress "Grading exercise 8." ;
  Section ([ Text "Exercise 8: " ; Code "print_filesystem" ],
           test_function_1_against_solution
             ~test_stdout ~test
             [%ty: filesystem -> unit] "print_filesystem"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ; exercise_5 ;
    exercise_6 ; exercise_7 ; exercise_8 ]
