open Report
open Test_lib

let canon_db db =
  let contacts = Array.map (fun _ -> nobody) db.contacts in
  let number_of_contacts =
    db.number_of_contacts
    |> min (Array.length db.contacts)
    |> max 0 in
  let useful = Array.sub db.contacts 0 number_of_contacts in
  Array.sort (fun a b -> compare b.name a.name) useful ;
  Array.blit useful 0 contacts 0 number_of_contacts ;
  { db with contacts }

let exercise_1 =
  Section ([ Text "Exercise 1:" ; Code "proof_of_bug" ],
           test_variable_property
             [%ty: query array ] "proof_of_bug"
             (fun proof_of_bug ->
                let db_wrong = ref (make 100) in
                let db_right = ref (make 100) in
                let rec apply = function
                  | [] ->
                      (if (canon_db !db_wrong) = (canon_db !db_right) then
                         [ Message ([ Text "Your sequence ends up with a database that seems corrupted." ; Break ;
                                      Text "That's a first step, \
                                            but you need to perform a last query that actually returns \
                                            a different result than expected to prove it."], Important) ]
                       else []) @
                      [ Message ([ Text "Your sequence does not exhibit the bug." ], Failure) ]
                  | ({ code = 0 | 1 | 2 as code} as query) :: queries ->
                      let (wrong_status, wrong_db, _) = original_engine !db_wrong query in
                      db_wrong := wrong_db ;
                      let (right_status, right_db, _) = Solution.engine !db_right query in
                      db_right := right_db ;
                      begin match wrong_status, right_status, code, queries with
                        | true, false, _, []
                        | false, true, _, [] ->
                            [ Message ([ Text "Congratulations, your sequence exhibits the bug!" ], Success 5) ]
                        | true, false, _, _ :: _
                        | false, true, _, _ :: _ ->
                            [ Message ([ Text "Your sequence exhibits a bug, \
                                               but it continues after that." ], Failure) ]
                        | _, _, _, [] ->
                            apply queries
                        | true, true, _, _ :: _
                        | false, false, 2, _ :: _ ->
                            apply queries
                        | false, false, 0, _ :: _ ->
                            [ Message ([ Text "Your sequence triggered a normal insertion failure." ], Failure) ]
                        | false, false, 1, _ :: _ ->
                            [ Message ([ Text "Your sequence triggered a normal deletion failure." ], Failure) ]
                        | false, false, _, _ :: _ -> assert false
                      end
                  | _ :: _ ->
                      [ Message ([ Text "Bad query code in your sequence." ], Failure) ] in
                apply (Array.to_list proof_of_bug)))

let sample_contact =
  let names = [|
    "ro" ; "ber" ; "to" ; "yann" ; "ralf" ; "gre" ;
    "goire" ; "ben" ; "ja" ; "min" ; "cag" ; "das" |] in
  let len = Array.length names in
  let id = ref (len + 1) in
  fun () ->
    let rec name id =
      if id < len then
        names.(id)
      else names.(id mod len) ^ name (id / len) in
    id := !id + len + 2 ;
    let name = String.capitalize_ascii (name !id) in
    let phone_number = (!id mod 23, !id mod 19, !id mod 7, !id mod 17) in
    { name ; phone_number }

let sampler_for_delete () =
  let size = Random.int 10 + 2 in
  let number_of_contacts = Random.int size in
  let cells i =
    if i >= number_of_contacts then nobody else sample_contact () in
  let db =
    { number_of_contacts ;
      contacts = Array.init size cells } in
  if number_of_contacts = 0 || Random.int 4 = 0 then
    db, sample_contact ()
  else
    db, db.contacts.(Random.int number_of_contacts)

let exercise_2 =
  Section ([ Text "Exercise 2: corrected " ; Code "delete" ],
           test_function_2_against_solution
             ~sampler: sampler_for_delete
             ~test:(test_eq_ok @@ fun (s, db, c) (s', db', c') -> s = s' && canon_db db = canon_db db')
             [%ty: database -> contact -> (bool * database * contact)] "delete"
             [ (let db = { number_of_contacts = 0 ; contacts = [||] } in
                db, sample_contact ()) ])

let sampler_for_update () =
  let size = Random.int 10 + 2 in
  let number_of_contacts = Random.int size in
  let cells i =
    if i >= number_of_contacts then nobody else sample_contact () in
  let db =
    { number_of_contacts ;
      contacts = Array.init size cells } in
  if number_of_contacts = 0 || Random.bool () then
    db, sample_contact ()
  else
    let contact = db.contacts.(Random.int number_of_contacts) in
    db, { contact with phone_number = (sample_contact ()).phone_number }

let exercise_3 =
  Section ([ Text "Exercise 3: " ; Code "update" ],
           test_function_2_against_solution
             ~sampler: sampler_for_update
             ~test:(test_eq_ok @@ fun (s, db, c) (s', db', c') -> s = s' && canon_db db = canon_db db')
             [%ty: database -> contact -> (bool * database * contact)] "update"
             [ (let db = { number_of_contacts = 0 ; contacts = [||] } in
                db, sample_contact ()) ])

let sampler_for_engine () =
  let size = Random.int 10 + 2 in
  let number_of_contacts = Random.int size in
  let cells i =
    if i >= number_of_contacts then nobody else sample_contact () in
  let db =
    { number_of_contacts ;
      contacts = Array.init size cells } in
  if number_of_contacts = 0 || Random.int 4 = 0 then
    db, { code = 1 ; contact = sample_contact () }
  else
    let contact = db.contacts.(Random.int number_of_contacts) in
    let contact = { contact with phone_number = (sample_contact ()).phone_number } in
    let code = Random.int 3 + 1 in
    db, { code ; contact }

let exercise_4 =
  Section ([ Text "Exercise 4: upgraded " ; Code "engine" ],
           let op = ref 0 in
           test_function_2_against_solution
             ~sampler: sampler_for_engine
             ~before_reference: (fun _ { code } -> op := code)
             ~test:(test_eq_ok @@ fun (s, db, c) (s', db', c') ->
                    s = s'
                    && canon_db db = canon_db db'
                    && (!op <> 2 || s = false || c = c'))
             [%ty: database -> query -> (bool * database * contact)] "engine"
             [])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ]
