let exec f x =
  match f x with
  | v -> Ok v
  | exception exn -> Error exn

let compare user reference to_string =
  match user, reference with
  | Ok u, Ok r when u = r ->
      ("got correct value " ^ to_string u , Successful)
  | Ok u, _ ->
      ("got unexpected value " ^ to_string u , Failed)
  | Error u, Error r when u = r ->
      ("got correct exception " ^ exn_to_string u , Successful)
  | Error u, _ ->
      ("got unexpected exception " ^ exn_to_string u , Failed)

let test user reference sample to_string =
  let rec loop n =
    if n = 0 then [] else
      let case = sample () in
      let user, reference = exec user case, exec reference case in
      compare user reference to_string :: loop (n - 1) in
  loop 10
