type 'a res = 'a result
type rep = report

open Report
open Test_lib

type int_int_fun = int -> int

exception Fail

let sample_int_int_fun =
  sample_cases
    [ printable_fun "fail_on_even" (fun x -> if x mod 2 = 0 then raise Fail else x) ;
      printable_fun "fail_on_odd" (fun x -> if x mod 2 = 1 then raise Fail else x) ;
      printable_fun "fail" (fun _ -> raise Fail) ]

let sample_int () =
  Random.int 6

type char_bool_fun = char -> bool

exception Out_of_range

let sample_char_bool_fun =
  sample_cases
    [ printable_fun "isuppercase" (function 'A'..'Z' -> true | 'a'..'z' -> false | _ -> raise Out_of_range) ;
      printable_fun "islowercase" (function 'a'..'z' -> true | 'A'..'Z' -> false | _ -> raise Out_of_range) ;
      printable_fun "isdigit" (function '0'..'9' -> true | _ -> false) ]

let sample_char () =
  match Random.int 3 with
  | 0 -> Char.chr (Char.code 'a' + Random.int 6 * 5)
  | 1 -> Char.chr (Char.code 'A' + Random.int 6 * 5)
  | _ -> Char.chr (Char.code '0' + Random.int 3 * 2 + 1)

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "exec" ],
           test_function_2_against_solution
             [%ty: int_int_fun -> int -> int res] "exec"
             [] @
           test_function_2_against_solution
             [%ty: char_bool_fun -> char -> bool res] "exec"
             [])


let sample_res sample () : _ res =
  if Random.bool () then Ok (sample ()) else
  if Random.bool () then Error Out_of_range else Error Fail

type int_to_string = int -> string
let sample_int_to_string () = printable_fun "string_of_int" string_of_int

type bool_to_string = bool -> string
let sample_bool_to_string () = printable_fun "string_of_bool" (Printf.sprintf "%b")

type char_to_string = char -> string
let sample_char_to_string () = printable_fun "string_of_char" (Printf.sprintf "%C")

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "compare" ],
           test_function_3_against_solution
             [%ty: char res -> char res -> char_to_string -> message] "compare"
             [] @
           test_function_3_against_solution
             [%ty: int res -> int res -> int_to_string -> message] "compare"
             [])

type int_sampler = unit -> int
let sample_int_sampler () =
  let rec build n = if n = 0 then [] else sample_int () :: build (n - 1) in
  let list = build 10 in
  let list = ref (list @ list) in
  printable_fun "sample_int" @@ fun () ->
  match !list with
  | [] -> assert false
  | [ last ] -> last
  | e :: es -> list := es ; e

type char_sampler = unit -> char
let sample_char_sampler () =
  let rec build n = if n = 0 then [] else sample_char () :: build (n - 1) in
  let list = build 10 in
  let list = ref (list @ list) in
  printable_fun "sample_char" @@ fun () ->
  match !list with
  | [] -> assert false
  | [ last ] -> last
  | e :: es -> list := es ; e

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "test" ],
           let after _ _ _ _ (got, _, _) (exp, _, _) =
             if List.length got <> List.length exp then
               [ Message ([ Text "The length of your result is wrong." ], Important) ]
             else if got = List.rev exp then
               [ Message ([ Text "Your result is right but in reverse order." ], Important) ]
             else if got <> exp then
               [ Message ([ Text "The expected report was:" ;
                            Code (Format.asprintf "%a" (Introspection.get_printer [%ty: rep]) exp) ], Important) ]
             else [] in
           test_function_4_against_solution ~after
             [%ty: char_bool_fun -> char_bool_fun -> char_sampler -> bool_to_string -> rep] "test"
             [] @
           test_function_4_against_solution ~after
             [%ty: int_int_fun -> int_int_fun -> int_sampler -> int_to_string -> rep] "test"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ]
