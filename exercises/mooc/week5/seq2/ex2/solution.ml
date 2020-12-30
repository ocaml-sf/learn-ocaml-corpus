let rec print_path = function
  | [] -> ()
  | [ last ] ->
      print_string last
  | dir :: path ->
      print_string dir ;
      print_char '/' ;
      print_path path

let rec print_file lvl name =
  if lvl = 0 then
    print_endline name
  else begin
    print_string "| " ;
    print_file (lvl - 1) name
  end

let rec print_symlink lvl name path =
  if lvl = 0 then begin
    print_string name ;
    print_string " -> " ;
    print_path path ;
    print_newline ()
  end else begin
    print_string "| " ;
    print_symlink (lvl - 1) name path
  end

let print_dir lvl name =
  print_file lvl ("/" ^ name)

let print_filesystem fs =
  let rec print_filesystem lvl = function
    | [] -> ()
    | (name, File) :: nodes ->
        print_file lvl name ;
        print_filesystem lvl nodes
    | (name, Symlink path) :: nodes ->
        print_symlink lvl name path ;
        print_filesystem lvl nodes
    | (name, Dir fs) :: nodes ->
        print_dir lvl name ;
        print_filesystem (lvl + 1) fs ;
        print_filesystem lvl nodes in
  print_filesystem 0 fs

let rec resolve sym path =
  let rec resolve acc = function
    | [] -> List.rev acc
    | "." :: rest -> resolve acc rest
    | ".." :: rest ->
        begin match acc with
        | [] -> resolve [] rest
        | _ :: acc -> resolve acc rest end
    | name :: rest -> resolve (name :: acc) rest in
  resolve (List.tl (List.rev sym)) path

let rec file_exists root path =
  match root, path with
  | (name, File) :: _, [ last ] when name = last -> true
  | (name, _) :: rest, item :: _ when item <> name -> file_exists rest path
  | (name, Dir fs) :: _, _ :: items -> file_exists fs items
  | _ -> false

let print_filesystem root =
  let rec print_filesystem lvl acc = function
    | [] -> ()
    | (name, File) :: nodes ->
        print_file lvl name ;
        print_filesystem lvl acc nodes
    | (name, Symlink path) :: nodes ->
        if file_exists root (resolve (List.rev (name :: acc)) path) then
          print_symlink lvl name path
        else
          print_symlink lvl name [ "INVALID" ] ;
        print_filesystem lvl acc nodes
    | (name, Dir fs) :: nodes ->
        print_dir lvl name ;
        print_filesystem (lvl + 1) (name :: acc) fs ;
        print_filesystem lvl acc nodes in
  print_filesystem 0 [] root
