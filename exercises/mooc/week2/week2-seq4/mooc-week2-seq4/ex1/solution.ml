let proof_of_bug =
  [| { code = 0 ; contact = { name = "luke"; phone_number = (1, 2, 3, 4) } } ;
     { code = 0 ; contact = { name = "darth"; phone_number = (4, 3, 2, 1) } } ;
     { code = 2 ; contact = { name = "luke"; phone_number = (1, 2, 3, 4) } } ;
     { code = 1 ; contact = { name = "luke"; phone_number = (4, 3, 2, 1) } } ;
     { code = 2 ; contact = { name = "luke"; phone_number = (1, 2, 3, 4) } } ;
     { code = 2 ; contact = { name = "darth"; phone_number = (1, 2, 3, 4) } } |]

let delete db contact =
  let shift_left idx =
    let cells i =
      if i < idx then db.contacts.(i)
      else if i + 1 < Array.length db.contacts then db.contacts.(i + 1)
      else nobody
    in
    let db' = {
      number_of_contacts = db.number_of_contacts - 1;
      contacts = Array.init (Array.length db.contacts) cells
    }
    in
    (true, db', contact)
  in
  let rec aux idx =
    if idx >= db.number_of_contacts then
      (false, db, contact)
    else if db.contacts.(idx).name = contact.name then
      shift_left idx
    else
      aux (idx + 1)
  in
  aux 0

let update db contact =
  let (status, db, _) = search db contact in
  if status then
    let cells i =
      if db.contacts.(i).name = contact.name then contact else db.contacts.(i)
    in
    let db' = {
      number_of_contacts = db.number_of_contacts;
      contacts = Array.init (Array.length db.contacts) cells
    }
    in
    (true, db', contact)
  else insert db contact

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;
