open Report
open Test_lib

let print_long_string ppf str =
  if String.length str > 200 then
    Format.fprintf ppf "%S" (String.sub str 0 200 ^ "...")
  else
    Format.fprintf ppf "%S" str

;;
#install_printer print_long_string
;;

let print_htable ppf htable =
  Format.fprintf ppf
    "(@[<hv 0>let table = Hashtbl.create %d in@,"
    (Hashtbl.length htable) ;
  Hashtbl.iter
    (fun w l ->
       Format.fprintf ppf "@[<hv 2>Hashtbl.add table %S @,%a ;@]@,"
         w (Introspection.get_printer [%ty: distribution]) l)
    htable ;
  Format.fprintf ppf  "table)@]"

;;
#install_printer print_htable
;;

let print_ptable_table ppf table =
  Format.fprintf ppf
    "(@[<hv 0>let table = Hashtbl.create %d in@,"
    (Hashtbl.length table) ;
  Hashtbl.iter
    (fun w l ->
       Format.fprintf ppf "@[<hv 2>Hashtbl.add table %a @,%a ;@]@,"
         (Introspection.get_printer [%ty: string list]) w
         (Introspection.get_printer [%ty: distribution]) l)
    table ;
  Format.fprintf ppf  "table)@]"

;;
#install_printer print_ptable_table
;;

let print_ptable ppf { prefix_length ; table } =
  Format.fprintf ppf
    "(@[<hv 0>let prefix_length = %d in@,let table = Hashtbl.create %d in@,"
    prefix_length (Hashtbl.length table) ;
  Hashtbl.iter
    (fun w l ->
       Format.fprintf ppf "@[<hv 2>Hashtbl.add table %a @,%a ;@]@,"
         (Introspection.get_printer [%ty: string list]) w
         (Introspection.get_printer [%ty: distribution]) l)
    table ;
  Format.fprintf ppf  "{ prefix_length ; table })@]"

;;
#install_printer print_ptable
;;

let graded n cb =
  match !graded_selection with
  | None -> cb ()
  | Some graded ->
      if List.mem n graded then cb ()
      else Report.[ Message ([ Text "Skipped."], Important) ]

type corpus = string
type words = string list

let corpus = [ simple_1 ; simple_2 ; simple_3 ]

let words =
  List.map Solution.words corpus

let ltables =
  List.map Solution.build_ltable words

let htables =
  List.map Solution.build_htable words

let sample_corpus =
  sample_cases corpus

let sample_words =
  sample_cases words

let sample_ltable =
  sample_cases ltables

let sample_htable =
  sample_cases htables

exception Report of Report.t

let to_string ty v =
  Format.asprintf "%a" (Introspection.get_printer ty) v

let test_dist ty got exp =
  let canon_dist { total ; amounts } =
    let rec aggregate acc = function
      | [] ->
          List.rev acc
      | (wa, ca) :: (wb, cb) :: rest when wa = wb ->
          aggregate acc ((wa, ca + cb) :: rest)
      | wc :: rest ->
          aggregate (wc :: acc) rest in
    let amounts = List.sort compare amounts in
    let amounts = aggregate [] amounts in
    { total ; amounts } in
  match got, exp with
  | Ok got, Ok exp ->
      let got = canon_dist got in
      let exp = canon_dist exp in
      let rec loop = function
        | [], [] ->
            [ Message ([
                  Text "Expected distribution" ;
                  Code (to_string ty got) ], Success 5) ]
        | (wgot, _) :: _, [] ->
            [ Message ([
                  Text "Invalid distribution" ;
                  Code (to_string ty got) ; Break ;
                  Text "Unexpected suffix" ;
                  Code (to_string [%ty: string] wgot) ], Failure) ]
        | (wgot, _) :: _, (wexp, _) :: _ when wgot < wexp ->
            [ Message ([
                  Text "Invalid distribution" ;
                  Code (to_string ty got) ; Break ;
                  Text "Unexpected suffix" ;
                  Code (to_string [%ty: string] wgot) ], Failure) ]
        | [], (wexp, _) :: _ ->
            [ Message ([
                  Text "Invalid distribution" ;
                  Code (to_string ty got) ; Break ;
                  Text "Missing suffix" ;
                  Code (to_string [%ty: string] wexp) ], Failure) ]
        | (wgot, _) :: _, (wexp, _) :: _ when wgot > wexp ->
            [ Message ([
                  Text "Invalid distribution" ;
                  Code (to_string ty got) ; Break ;
                  Text "Missing suffix" ;
                  Code (to_string [%ty: string] wexp) ], Failure) ]
        | (wgot, ngot) :: _, (wexp, nexp) :: _ when ngot <> nexp ->
            [ Message ([
                  Text "Invalid distribution" ;
                  Code (to_string ty got) ; Break ;
                  Text "For suffix" ;
                  Code (to_string [%ty: string] wexp) ;
                  Text (Format.asprintf "' I have %d occurences, you have %d occurences" nexp ngot) ], Failure) ]
        | _ :: gots, _ :: exps ->
            loop (gots, exps) in
      if got.total <> exp.total then
        [ Message ([
              Text "Invalid distribution." ; Break ;
              Text (Format.asprintf "My total is %d, yours is %d" exp.total got.total) ], Failure) ]
      else
        loop (got.amounts, exp.amounts)
  | _, _ -> test_ignore ty got exp

let test_htable pty ty got exp =
  let canon_htable t =
    let r = Hashtbl.create (Hashtbl.length t) in
    Hashtbl.iter (fun k v ->
        try ignore (Hashtbl.find r k) with Not_found ->
          Hashtbl.add r k v) t ; r in
  match got, exp with
  | Ok got, Ok exp ->
      let got = canon_htable got in
      let exp = canon_htable exp in
      begin try
          if Hashtbl.length got > Hashtbl.length exp then begin
            Hashtbl.iter (fun k _ ->
                try ignore (Hashtbl.find exp k) with Not_found ->
                  raise (Report [ Message ([
                      Text "Invalid table" ;
                      Code (to_string ty got) ; Break ;
                      Text "Unexpected prefix" ;
                      Code (to_string pty k) ], Failure) ]))
              got
          end ;
          if Hashtbl.length got < Hashtbl.length exp then begin
            Hashtbl.iter (fun k _ ->
                try ignore (Hashtbl.find got k) with Not_found ->
                  raise (Report [ Message ([
                      Text "Invalid table" ;
                      Code (to_string ty got) ; Break ;
                      Text "Missing prefix" ;
                      Code (to_string pty k) ], Failure) ]))
              exp
          end ;
          Hashtbl.iter
            (fun k v ->
               try
                 let report = test_dist [%ty: distribution] (Ok v) (Ok (Hashtbl.find exp k)) in
                 if snd (Report.result report) then
                   raise (Report (
                       [ Message ([
                             Text "Invalid table" ;
                             Code (to_string ty got) ; Break ;
                             Text "Bad distribution for prefix" ;
                             Code (to_string pty k) ], Failure) ] @
                       report))
               with Not_found ->
                 raise (Report [ Message ([
                     Text "Invalid table" ;
                     Code (to_string ty got) ; Break ;
                     Text "Unexpected prefix" ;
                     Code (to_string pty k) ], Failure) ]))
            got ;
          [ Message ([ Text "Expected table" ;
                       Code (to_string ty got) ], Success 5) ]
        with
          Report r -> r end
  | _, _ -> test_ignore ty got exp

let test_ptable ty got exp =
  match got, exp with
  | Ok got, Ok exp ->
      if got.prefix_length <> got.prefix_length then
        [ Message ([
              Text "Invalid prefix length" ;
              Code (string_of_int got.prefix_length) ], Failure) ]
      else
        test_htable
          [%ty: string list] [%ty: (string list, distribution) Hashtbl.t]
          (Ok got.table) (Ok exp.table)
  | _, _ -> test_ignore ty got exp

let test_ltable ty got exp =
  let canon_ltable ltable =
    let ltable = List.mapi (fun i (w, l) -> (w, i, List.sort compare l)) ltable in
    let ltable = List.sort compare ltable in
    let rec uniq acc = function
      | [] -> List.rev acc
      | [ (wo, _, lo) ] -> List.rev ((wo, lo) :: acc)
      | (wa, _, _) as a :: (wb, _, _) :: rest when wa = wb -> uniq acc (a :: rest)
      | (wa, _, la) :: rest -> uniq ((wa, la) :: acc) rest in
    uniq [] ltable in
  match got, exp with
  | Ok got, Ok exp ->
      let got = canon_ltable got in
      let exp = canon_ltable exp in
      let rec loop = function
        | [], [] ->
            [ Message ([
                  Text "Expected table" ;
                  Code (to_string ty got) ], Success 5) ]
        | (wgot, _) :: _, [] ->
            [ Message ([
                  Text "Invalid table" ;
                  Code (to_string ty got) ; Break ;
                  Text "Unexpected prefix" ;
                  Code (to_string [%ty: string] wgot) ], Failure) ]
        | (wgot, _) :: _, (wexp, _) :: _ when wgot < wexp ->
            [ Message ([
                  Text "Invalid table" ;
                  Code (to_string ty got) ; Break ;
                  Text "Unexpected prefix" ;
                  Code (to_string [%ty: string] wgot) ], Failure) ]
        | [], (wexp, _) :: _ ->
            [ Message ([
                  Text "Invalid table" ;
                  Code (to_string ty got) ; Break ;
                  Text "Missing prefix" ;
                  Code (to_string [%ty: string] wexp) ], Failure) ]
        | (wgot, _) :: _, (wexp, _) :: _ when wgot > wexp ->
            [ Message ([
                  Text "Invalid table" ;
                  Code (to_string ty got) ; Break ;
                  Text "Missing prefix" ;
                  Code (to_string [%ty: string] wexp) ], Failure) ]
        | (wgot, ngot) :: gots, (_, nexp) :: exps ->
            let ngot = Solution.compute_distribution ngot in
            let nexp = Solution.compute_distribution nexp in
            let report = test_dist [%ty: distribution] (Ok ngot) (Ok nexp) in
            if snd (Report.result report) then
              [ Message ([
                    Text "Invalid table" ;
                    Code (to_string ty got) ; Break ;
                    Text "Bad distribution for prefix" ;
                    Code (to_string [%ty: string] wgot) ], Failure) ] @
              report
            else
              loop (gots, exps) in
      loop (got, exp)
  | _, _ -> test_ignore ty got exp

let test_distribution table next =
  let test_on_corpus corpus =
    let words = Solution.words corpus in
    let htable = table words in
    let ltable = Solution.build_ltable words in
    let reports = List.map (fun (w, sufs) ->
        let dom = List.sort_uniq compare sufs in
        let dist sam =
          let nb = List.length sam in
          List.map
            (fun w -> w, List.length (List.filter ((=) w) sam) * 100 / nb)
            dom in
        let rec sample acc i =
          if i = 0 then acc else sample (next htable w :: acc) (i - 1) in
        let exp = dist sufs in
        let got = dist (sample [] 1000) in
        let text =
          List.map
            (fun (n, pct) ->
               [ Break ; Code (Format.asprintf "%S -> %S %d%%" w n pct) ])
            got |> List.flatten in
        if List.for_all2 (fun (_, exp) (_, got) -> abs (exp - got) < 8) exp got then
          [ Message ([ Text "Expected distribution for" ; Code ("\"" ^ w ^ "\"") ] @ text, Informative) ]
        else
          [ Message ([ Text "Unexpected distribution for" ; Code ("\"" ^ w ^ "\"") ] @ text, Failure) ])
        ltable in
    let report = List.flatten reports in
    [ Message ([ Text "Testing on" ;  Code ("\"" ^ corpus^ "\"") ], Informative) ] @
    if snd (Report.result report) then report
    else [ Message ([ Text "Expected distribution" ], Success 1) ] @ report in
  List.flatten @@ List.map test_on_corpus
    [ "a b a c a" ;
      "a b a c a c a" ]

let test_sentence suffix pruffix got =
  let rec check acc = function
    | [] -> Message ([ Text "Empty sentence." ], Failure)
    | [ last ] ->
        if suffix (last :: acc) "STOP" then
          Message ([ Text "Correct sequence." ], Success 1)
        else
          Message ([ Text "Bad terminator \"" ; Text last ; Text "\"" ], Failure)
    | wa :: (wb :: _ as rest) ->
        let acc = wa :: acc in
        if suffix acc wb then
          check acc rest
        else if suffix acc "STOP" then
          check [] rest
        else
          Message ([ Text "Bad sequence" ; Text (pruffix acc) ; Text "->" ; Text wb ], Failure) in
  let txt = String.concat " " got in
  [ Message ([ Text "Checking" ; Text txt ], Informative) ;
    check [] got ]

let test_suffix suffix word got =
  if suffix [ word ] got then
    [ Message ([ Text "Expected word" ; Text got ], Success 1) ]
  else
    [ Message ([ Text "Unexpected word" ; Text got ], Failure) ]

let rec trunc acc = function
  | (0, _) | (_, []) -> acc
  | (n, e :: es) -> trunc (e :: acc) (n - 1, es)

let exercise_1 =
  let test ty got exp =
    let rgots = ref [] in
    let rexps = ref [] in
    let dump () =
      rgots := trunc [] (5, !rgots) ;
      rexps := trunc [] (5, !rexps) ;
      let rec pp_words ppf = function
        | [] -> ()
        | x :: xs ->
            Format.fprintf ppf " %S%a" x pp_words xs in
      Format.asprintf
        "@[<v 0>my last words were:@,@[<hov 2> ...%a@]@,\
         your last words were:@,@[<hov 2> ...%a@]@."
        pp_words !rexps
        pp_words !rgots in
    let rec words i = function
      | [], [] ->
          [ Message ([ Text "Text splitted as expected." ], Success 5)]
      | [], exp :: r ->
          rexps := (match r with [] -> exp :: !rexps | _ -> "..." :: exp :: !rexps) ;
          let msg = "I found more words than you." in
          [ Message ([ Text msg ; Break ; Code (dump ()) ], Failure)]
      | got :: r, [] ->
          rgots := (match r with [] -> got :: !rgots | _ -> "..." :: got :: !rgots) ;
          let msg = "You found more words than me." in
          [ Message ([ Text msg ; Break ; Code (dump ()) ], Failure)]
      | got :: gots, exp :: exps when got = exp ->
          rgots := got :: !rgots ;
          rexps := exp :: !rexps ;
          words (i + 1) (gots, exps)
      | got :: _, exp :: _ ->
          rgots := got :: !rgots ;
          rexps := exp :: !rexps ;
          let msg = Format.asprintf
              "Your word %d is %S, mine is %S." i got exp in
          [ Message ([ Text msg ; Break ; Code (dump ()) ], Failure)] in
    match got, exp with
    | Ok got, Ok exp -> words 1 (got, exp)
    | _, _ -> test_ignore ty got exp in
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "words" ],
           graded 1 @@ fun () ->
           test_function_1_against_solution ~test
             [%ty: corpus -> words] "words"
             [])


let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "build_ltable" ],
           graded 2 @@ fun () ->
           test_function_1_against_solution
             ~test: test_ltable
             [%ty: words -> ltable] "build_ltable"
             [])

let suffix_ltable ltable acc suf =
  try List.mem suf (List.assoc (List.hd acc) ltable)
  with Not_found -> false

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "next_in_ltable" ],
           graded 3 @@ fun () ->
           let sampler () =
             let ltable = sample_ltable () in
             ltable, fst (List.nth ltable (Random.int (List.length ltable))) in
           let report = test_function_2_against_solution
               ~test: test_ignore
               ~after: (fun ltable word (got, _, _) _ ->
                   test_suffix (suffix_ltable ltable) word got)
               ~sampler
               [%ty: ltable -> string -> string] "next_in_ltable"
               [] in
           if snd (Report.result report) then report else
             report @ test_variable_property
               [%ty: ltable -> string -> string] "next_in_ltable" @@ fun next ->
             [ Message ([ Text "Now I will check the probabilities."], Important) ] @
             test_distribution Solution.build_ltable next)

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "walk_ltable" ],
           graded 4 @@ fun () ->
           test_function_1_against
             ~test: test_ignore
             ~after: (fun ltable (got, _, _) _ ->
                 test_sentence (suffix_ltable ltable) (fun l -> String.concat " " l) got)
             [%ty: ltable -> string list] "walk_ltable"
             (fun _ -> []) [])

let sample_abc = sample_cases [ "a" ; "b" ; "c" ]

let exercise_5 =
  set_progress "Grading exercise 5." ;
  Section ([ Text "Exercise 5: " ; Code "compute_distribution" ],
           graded 5 @@ fun () ->
           test_function_1_against_solution
             ~sampler: (fun () -> sample_list ~min_size: 10 ~max_size: 40 sample_abc ())
             ~test: test_dist
             [%ty: string list -> distribution] "compute_distribution"
             [])

let suffix_htable table acc suf =
  try
    let { amounts } = Hashtbl.find table (List.hd acc) in
    List.mem_assoc suf amounts
  with Not_found -> false

let exercise_6 =
  set_progress "Grading exercise 6." ;
  Section ([ Text "Exercise 6: " ; Code "build_htable" ],
           graded 6 @@ fun () ->
           test_function_1_against_solution
             ~test: (test_htable [%ty: string])
             [%ty: words -> htable] "build_htable"
             [])

let exercise_7 =
  set_progress "Grading exercise 7." ;
  Section ([ Text "Exercise 7: " ; Code "next_in_htable" ],
           graded 7 @@ fun () ->
           let sampler () =
             let htable = sample_htable () in
             let pre = ref "START" in
             let i = ref (Random.int (Hashtbl.length htable)) in
             Hashtbl.iter (fun k _ -> if !i = 0 then pre := k ; decr i) htable ;
             htable, !pre in
           let report = test_function_2_against_solution
               ~test: test_ignore
               ~after: (fun htable word (got, _, _) _ ->
                   test_suffix (suffix_htable htable) word got)
               ~sampler
               [%ty: htable -> string -> string] "next_in_htable"
               [] in
           if snd (Report.result report) then report else
             report @ test_variable_property
               [%ty: htable -> string -> string] "next_in_htable" @@ fun next ->
             [ Message ([ Text "Now I will check the probabilities."], Important) ] @
             test_distribution Solution.build_htable next)

let exercise_8 =
  set_progress "Grading exercise 8." ;
  Section ([ Text "Exercise 8: " ; Code "walk_htable" ],
           graded 8 @@ fun () ->
           test_function_1_against
             ~test: test_ignore
             ~after: (fun htable (got, _, _) _ ->
                 test_sentence (suffix_htable htable) (fun l -> String.concat " " l) got)
             [%ty: htable -> string list] "walk_htable"
             (fun _ -> []) [])

let sample_corpus =
  sample_cases
    [ grimms_travelling_musicians ;
      grimms_cat_and_mouse_in_partnership ;
      some_cookbook_sauce_chapter ;
      history_of_ocaml ;
      simple_1 ; simple_2 ; simple_3 ;
      multi_1 ; multi_2 ; multi_3 ]

let exercise_9 =
  let test ty got exp =
    let rgots = ref [] in
    let rexps = ref [] in
    let dump () =
      rgots := trunc [] (5, !rgots) ;
      rexps := trunc [] (5, !rexps) ;
      let rec pp_words ppf = function
        | [] -> ()
        | x :: xs ->
            Format.fprintf ppf " %S%a" x pp_words xs in
      Format.asprintf
        "@[<v 0>my last words were:@,@[<hov 2> ...%a@]@,\
         your last words were:@,@[<hov 2> ...%a@]@."
        pp_words !rexps
        pp_words !rgots in
    let rec sentences i = function
      | [], [] ->
          [ Message ([ Text "Text splitted as expected." ], Success 5)]
      | [], _ ->
          [ Message ([ Text "I found more sentences than you." ;
                       Break ; Code (dump ()) ], Failure)]
      | _, [] ->
          [ Message ([ Text "You found more sentences than me." ;
                       Break ; Code (dump ()) ], Failure)]
      | [] :: gots, [] :: exps -> (* absurd *)
          sentences (i + 1) (gots, exps)
      | gots, [] :: exps -> (* absurd *)
          sentences i (gots, exps)
      | [] :: _, _ ->
          [ Message ([ Text "Unexpected empty sentence in your result." ;
                       Break ; Code (dump ()) ], Failure)]
      | got :: gots, exp :: exps ->
          words i 1 (gots, exps) (got, exp)
    and words i j cont = function
      | [], [] ->
          sentences (i + 1) cont
      | [], exp :: r ->
          rexps := (match r with [] -> exp :: !rexps | _ -> "..." :: exp :: !rexps) ;
          let msg = Format.asprintf
              "In sentence %d, I found more words than you." i in
          [ Message ([ Text msg ; Break ; Code (dump ()) ], Failure)]
      | got :: r, [] ->
          rgots := (match r with [] -> got :: !rgots | _ -> "..." :: got :: !rgots) ;
          let msg = Format.asprintf
              "In sentence %d, you found more words than me." i in
          [ Message ([ Text msg ; Break ; Code (dump ()) ], Failure)]
      | got :: gots, exp :: exps when got = exp ->
          rgots := got :: !rgots ;
          rexps := exp :: !rexps ;
          words i (j + 1) cont (gots, exps)
      | got :: _, exp :: _ ->
          rgots := got :: !rgots ;
          rexps := exp :: !rexps ;
          let msg = Format.asprintf
              "In sentence %d, your word %d is %S, mine is %S." i j got exp in
          [ Message ([ Text msg ; Break ; Code (dump ()) ], Failure)] in
    match got, exp with
    | Ok got, Ok exp ->
        sentences 1 (got, exp)
    | _, _ -> test_ignore ty got exp in
  set_progress "Grading exercise 9." ;
  Section ([ Text "Exercise 9: " ; Code "sentences" ],
           graded 9 @@ fun () ->
           test_function_1_against_solution ~test
             [%ty: corpus -> string list list] "sentences"
             [])

let exercise_10 =
  set_progress "Grading exercise 10." ;
  Section ([ Text "Exercise 10: " ; Code "start" ],
           graded 10 @@ fun () ->
           test_function_1_against_solution ~gen: 0
             [%ty: int -> string list] "start"
             [ 0 ; 1 ; 2 ; 3 ; 4 ])

let exercise_11 =
  set_progress "Grading exercise 11." ;
  Section ([ Text "Exercise 11: " ; Code "shift" ],
           graded 11 @@ fun () ->
           test_function_2_against_solution ~gen: 0
             [%ty: string list -> string -> string list] "shift"
             [ [ "A" ; "B" ; "C" ], "D" ;
               [ "B" ; "C" ; "D" ], "E" ;
               [ "before" ], "after" ;
               [ "an" ; "never" ; "gonna" ], "give" ])

let sample_words =
  sample_cases
    (List.flatten
       [ Solution.sentences simple_1 ;
         Solution.sentences simple_2 ;
         Solution.sentences simple_3 ;
         Solution.sentences multi_1 ;
         Solution.sentences multi_2 ;
         Solution.sentences multi_3 ])

let sample_int () =
  Random.int 2 + 1

let exercise_12 =
  set_progress "Grading exercise 12." ;
  Section ([ Text "Exercise 12: " ; Code "build_ptable" ],
           graded 12 @@ fun () ->
           test_function_2_against_solution
             ~test: test_ptable
             [%ty: words -> int -> ptable] "build_ptable"
             [])

let trunc_prefix ptable l =
  trunc [] (ptable.prefix_length,
            l @ Solution.start ptable.prefix_length)

let suffix_ptable ({ table } as ptable) acc suf =
  try
    let acc = trunc_prefix ptable acc in
    let { amounts } = Hashtbl.find table acc in
    List.mem_assoc suf amounts
  with Not_found -> false

let sample_ptable () =
  let pl = Random.int 2 + 1 in
  Solution.merge_ptables
    [ Solution.build_ptable (sample_words ()) pl ;
      Solution.build_ptable (sample_words ()) pl ]

let exercise_13 =
  set_progress "Grading exercise 13." ;
  Section ([ Text "Exercise 13: " ; Code "walk_ptable" ],
           graded 13 @@ fun () ->
           test_function_1_against
             ~test: test_ignore
             ~after: (fun ptable (got, _, _) _ ->
                 test_sentence (suffix_ptable ptable)
                   (fun l -> String.concat " " (trunc_prefix ptable l)) got)
             [%ty: ptable -> string list] "walk_ptable"
             (fun _ -> []) [])

let sample_ptable () =
  Solution.build_ptable (sample_words ()) 2

let sample_list s () = sample_list ~min_size: 1 ~max_size: 4 s ()

let exercise_14 =
  set_progress "Grading exercise 14." ;
  Section ([ Text "Exercise 14: " ; Code "merge_ptables" ],
           graded 14 @@ fun () ->
           test_function_1_against_solution
             ~test: test_ptable
             [%ty: ptable list -> ptable] "merge_ptables"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  begin match !graded_selection with
    | None -> []
    | Some _ -> [ Message ([ Text "Some exercises have been skipped." ], Failure) ]
  end @
  [ (* Part A *)
    exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ;
    (* Part B *)
    exercise_5 ; exercise_6 ; exercise_7 ; exercise_8 ;
    (* Part C *)
    exercise_9 ; exercise_10 ; exercise_11 ; exercise_12 ; exercise_13 ; exercise_14 ]
