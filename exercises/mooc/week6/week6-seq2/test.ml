open Report
open Test_lib

module type Code_S = sig
  module Dict : DictSig
end

let universe =
  [ "Eggplant" ; "Zucchini" ; "Salad" ; "Carrot" ; "Radish" ;
    "Apple" ; "Banana" ; "Tomato" ; "Bean" ; "Orange" ]
let sample_string = sample_cases universe

let safe_assoc d k = try Some (List.assoc k d) with Not_found -> None
let reify_set d = List.map (fun k -> k, safe_assoc d k) universe

let diff exp r =
  List.fold_left2
    (fun acc (k, exp) r ->
       if exp = r then acc else (k, exp, r) :: acc)
    []
    exp
    r

let rec sample_sequence set seq len =
  match Random.int len, seq, set with
  | 0, _ :: _, _ :: _ -> List.rev seq, set
  | _, _, _ :: _ when Random.bool () ->
      let n = fst (sample_cases set ()) in
      sample_sequence
        (List.filter (fun (n', _) -> n' <> n) set)
        (`Del n :: seq) (len - 1)
  | _ ->
      let rec neu s =
        let n = sample_string () in
        if List.mem_assoc n s then neu s else n in
      let n = neu set in
      let v = Random.int 11 in
      sample_sequence ((n, v) :: set) (`Add (n, v) :: seq) (len - 1)

let sample_sequence () = sample_sequence [] [] 10

let rec message ppf = function
  | [] -> ()
  | `Add (n, v) :: r ->
      Format.fprintf ppf "let d = Dict.add d %S %d in@,%a" n v message r
  | `Del n :: r ->
      Format.fprintf ppf "let d = Dict.remove d %S in@,%a" n message r

let display_error k exp found =
  let key = Code ("\"" ^ k ^ "\"") in
  match exp, found with
  | None, Some found ->
      [ Text "For the key" ; key ; Text "you returned" ;
        Code (string_of_int found) ; Text "while the exception" ;
        Code "NotFound" ; Text "was expected." ]
  | Some exp, None ->
      [ Text "For the key" ; key ; Text "you returned" ;
        Text "the exception" ; Code "NotFound" ; Text "when the value" ;
        Code (string_of_int exp) ; Text "was expected." ]
  | Some exp, Some found ->
      [ Text "For the key" ; key ; Text "you returned" ;
        Code (string_of_int found) ; Text "when the value" ;
        Code (string_of_int exp) ; Text "was expected." ]
  | None, None ->
      [ Text "Internal error." ]

let test_exo1 (module Code: Code_S) =
  let reify_dict d =
    let safe_lookup d k =
      try Some (Code.Dict.lookup d k) with Code.Dict.NotFound -> None in
    List.map (safe_lookup d) universe in
  let rec run_student_code d = function
    | [] -> reify_dict d
    | `Add (n, v) :: r -> run_student_code (Code.Dict.add d n v) r
    | `Del n :: r -> run_student_code (Code.Dict.remove d n) r in
  let test_dict r exp =
    match r with
    | Error exn ->
        Report.[ Message ([ Text "Wrong exception" ;
                            Code (Printexc.to_string exn) ],
                          Failure) ]
    | Ok r ->
        match diff exp r with
        | []  ->
            Report.[ Message ([ Text "Correct dictionnary returned." ],
                              Success 1) ]
        | (k, exp, found) :: _ ->
            Report.[ Message ( Text "Unexpected dictionnary returned." ::
                               display_error k exp found,
                               Failure) ;] in
  let rec test (seq, exp) =
    [ Message ([ Text "Computing the following sequence:" ; Break;
                 Code (Format.asprintf
                         "@[<v 0>let d = Dict.empty in@,%ad@."
                         message seq) ], Informative) ] @
    test_dict
      (result (fun () -> run_student_code Code.Dict.empty seq))
      (reify_set exp) in
  let tests =
    ([`Add ("Eggplant", 5) ; `Del "Eggplant"], []) ::
    ([`Add ("Eggplant", 5) ; `Del "Zucchini"], ["Eggplant", 5]) ::
    ([`Add ("Eggplant", 5) ; `Add ("Zucchini", 3) ; `Del "Zucchini"],
     ["Eggplant", 5]) ::
    ([`Add ("Eggplant", 5) ; `Add ("Zucchini", 3) ; `Del "Eggplant"],
     ["Zucchini", 3]) ::
    ([`Add ("Eggplant", 5) ; `Add ("Zucchini", 3) ;  `Add ("Banana", 1) ; `Del "Eggplant"],
     ["Banana", 1 ; "Zucchini", 3]) ::
    ([`Add ("Eggplant", 5) ; `Add ("Zucchini", 3) ;  `Add ("Banana", 1) ; `Del "Apple"],
     ["Banana", 1 ; "Eggplant", 5 ; "Zucchini", 3]) ::
    Array.(to_list (init 14 (fun _ -> sample_sequence ()))) in
  List.flatten (List.map test tests)

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "Dict" ],
           test_variable_property [%ty: (module DictSig)] "Dict" @@ fun _ ->
           test_student_code [%ty: (module Code_S)] test_exo1)

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ]
