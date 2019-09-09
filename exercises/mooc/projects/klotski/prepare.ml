module IntListSet = Set.Make (struct type t = int list let compare = compare end)
type int_list_set_operations = (int list, IntListSet.t) set_operations
let int_list_set_operations =
  IntListSet.{ empty ; add ; mem }
let print_int_list_set ppf int_set =
  let print_int_list ppf =
    Format.fprintf ppf "[@[<hov 0>%a]@]"
      (Format.pp_print_list
         ~pp_sep: (fun ppf () -> Format.fprintf ppf ";")
         (fun ppf -> Format.fprintf ppf "%d")) in
  Format.fprintf ppf "(@[<hov 2>int_list_set_of_list [%a]@])"
    (Format.pp_print_list
       ~pp_sep: (fun ppf () -> Format.fprintf ppf ";")
       print_int_list)
    (IntListSet.elements int_set) ;;
#install_printer print_int_list_set ;;

module IntSet = Set.Make (struct type t = int let compare = (-) end)
type int_set_operations = (int, IntSet.t) set_operations
let int_set_operations =
  IntSet.{ empty ; add ; mem }
let print_int_set ppf int_set =
  Format.fprintf ppf "(@[<hov 2>int_set_of_list [%a]@])"
    (Format.pp_print_list
       ~pp_sep: (fun ppf () -> Format.fprintf ppf ";")
       (fun ppf -> Format.fprintf ppf "%d"))
    (IntSet.elements int_set) ;;
#install_printer print_int_set ;;

module Anim = struct
  type point = float * float
  type style =
    { fill: string option ;
      stroke: (float * string) option ;
      blur: float ;
      shadow: (float * float * float * string) option ;
      opacity: float }
  type transform =
    { move: float * float ;
      rotate: float }
  let identity =
    { move = (0., 0.) ; rotate = 0. }
  let style ?fill ?stroke ?(blur = 0.) ?shadow ?(opacity = 1.) () =
    { fill ; stroke ; blur ; shadow ; opacity }
  let transform ?(move = (0., 0.)) ?(rotate = 0.) () =
    { move ; rotate }
  type shape =
    [ `Circle of float
    | `Rect of float * float
    | `Path of [ `Move_to of point
               | `Line_to of point
               | `Cubic_to of point * point
               | `Quadratic_to of point * point * point ] list
    | `Group of (shape * transform * style) list ]

  type image = ((string * shape) * transform * style) list
  type timeline = (float * image) list
  type animation = (int * int) * timeline list
  let uid = ref 100
  let uid shape = incr uid ; ("s" ^ string_of_int !uid, shape)

  let print ppf ((w, h), tmls) =
    let open Format in
    let module SM = Map.Make (String) in
    let print_prop ppf k v = match k with
      | "filter" | "animation" | "animation-fill-mode" ->
          fprintf ppf "@,-webkit-%s: %s;" k v ;
          fprintf ppf "@,%s: %s;" k v
      | _ -> fprintf ppf "@,%s: %s;" k v in
    let print_fixed_props ppf
        ({ move = (x, y) ; rotate = r },
         { fill = f ; stroke = s ; blur = bl ; shadow = sh ; opacity = op }) =
      let props = Hashtbl.create 100 in
      let set n v =
        try let p = Hashtbl.find props n in
          Hashtbl.replace props n (p ^ " " ^ v)
        with Not_found ->
          Hashtbl.replace props n v in
      if x <> 0. || y <> 0. then begin
        set "transform" (Format.asprintf "translate(%gpx,%gpx)" x y)
      end ;
      if r <> 0. then begin
        set "transform" (Format.asprintf "rotate(%gdeg)" r)
      end ;
      begin match f with
        | None -> ()
        | Some f -> set "fill" f
      end ;
      begin match s with
        | None -> ()
        | Some (w, s) ->
            set "stroke" s ;
            set "stroke-width" (asprintf "%gpx" w)
      end ;
      if op <> 1. then begin
        set "opacity" (string_of_float op)
      end ;
      if bl <> 0. then begin
        set "filter" (Format.asprintf "blur (%gpx)" op)
      end ;
      begin match sh with
        | None -> ()
        | Some (dx, dy, w, s) ->
            set "filter"
              (Format.asprintf "drop-shadow(%gpx %gpx %gpx %s)" dx dy w s) ;
      end ;
      Hashtbl.iter (fun k v -> fprintf ppf "%s: %s;" k v) props in
    let print_anim ppf (id, shape, tml, sm, maxt) =
      let rec compute_anim = function
        | [] | [ _ ] -> []
        | (ta, imga) :: ((tb, imgb) :: _ as rest) ->
            let pa = try Some (List.find (fun ((id', _), _, _) -> id = id') imga) with Not_found -> None in
            let pb = try Some (List.find (fun ((id', _), _, _) -> id = id') imgb) with Not_found -> None in
            let props = Hashtbl.create 10 in
            let set n (va, vb) =
              try let a, b = Hashtbl.find props n in
                Hashtbl.replace props n (a ^ " " ^ va, b ^ " " ^ vb)
              with Not_found ->
                Hashtbl.replace props n (va, vb) in
            begin match pa, pb with
              | Some (_, { move = (xa, ya) ; rotate = ra },
                      { fill = fa ; stroke = sa ; blur = bla ; shadow = sha ; opacity = opa }),
                Some (_, { move = (xb, yb) ; rotate = rb },
                      { fill = fb ; stroke = sb ; blur = blb ; shadow = shb ; opacity = opb }) ->
                  if xa <> 0. || xb <> 0. || ya <> 0. || yb <> 0. then begin
                    set "transform"
                      (Format.asprintf "translate(%gpx,%gpx)" xa ya,
                       Format.asprintf "translate(%gpx,%gpx)" xb yb)
                  end ;
                  if ra <> 0. || rb <> 0. then begin
                    set "transform"
                      (Format.asprintf "rotate(%gdeg)" ra,
                       Format.asprintf "rotate(%gdeg)" rb)
                  end ;
                  begin match fa, fb with
                    | None, None -> ()
                    | None, Some fb -> set "fill" ("transparent", fb)
                    | Some fa, None -> set "fill" (fa, "transparent")
                    | Some fa, Some fb -> set "fill" (fa, fb)
                  end ;
                  begin match sa, sb with
                    | None, None -> ()
                    | None, Some (wb, sb) ->
                        set "stroke" ("transparent", sb) ;
                        set "stroke-width" (asprintf "%gpx" wb, asprintf "%gpx" wb)
                    | Some (wa, sa), None ->
                        set "stroke" (sa, "transparent") ;
                        set "stroke-width" (asprintf "%gpx" wa, asprintf "%gpx" wa)
                    | Some (wa, sa), Some (wb, sb) ->
                        set "stroke" (sa, sb) ;
                        set "stroke-width" (asprintf "%gpx" wa, asprintf "%gpx" wb)
                  end ;
                  if opa <> 1. || opb <> 1. then begin
                    set "opacity" (string_of_float opa, string_of_float opb)
                  end ;
                  if bla <> 0. || blb <> 0. then begin
                    set "filter"
                      (Format.asprintf "blur (%gpx)" opa,
                       Format.asprintf "blur (%gpx)" opb)
                  end ;
                  begin match sha, shb with
                    | None, None -> ()
                    | None, Some (dxb, dyb, wb, sb) ->
                        set "filter"
                          (Format.asprintf "drop-shadow(%gpx %gpx %gpx transparent)" dxb dyb wb,
                           Format.asprintf "drop-shadow(%gpx %gpx %gpx %s)" dxb dyb wb sb) ;
                    | Some (dxa, dya, wa, sa), None ->
                        set "filter"
                          (Format.asprintf "drop-shadow(%gpx %gpx %gpx %s)" dxa dya wa sa,
                           Format.asprintf "drop-shadow(%gpx %gpx %gpx transparent)" dxa dya wa) ;
                    | Some (dxa, dya, wa, sa), Some (dxb, dyb, wb, sb) ->
                        set "filter"
                          (Format.asprintf "drop-shadow(%gpx %gpx %gpx %s)" dxa dya wa sa,
                           Format.asprintf "drop-shadow(%gpx %gpx %gpx %s)" dxb dyb wb sb) ;
                  end
              | _ ->
                  set "opacity" ("0", "0")
            end ;
            (ta, tb, props) :: compute_anim rest in
      let rec do_anim = function
        | [] -> ()
        | [ (ta, tb, props) ] ->
            if Hashtbl.length props <> 0 then begin
              fprintf ppf "@,@[<v 2>%g%% {" (100. *. ta /. maxt) ;
              Hashtbl.iter (fun k (v, _) -> print_prop ppf k v) props ;
              fprintf ppf "@]@,}" ;
              fprintf ppf "@,@[<v 2>100%% {" ;
              Hashtbl.iter (fun k (_, v) -> print_prop ppf k v) props ;
              fprintf ppf "@]@,}"
            end
        | (ta, tb, props) :: ((_, _, props') :: _ as rest) ->
            if Hashtbl.length props <> 0 then begin
              fprintf ppf "@,@[<v 2>%g%% {" (100. *. ta /. maxt) ;
              Hashtbl.iter (fun k (v, _) -> print_prop ppf k v) props ;
              fprintf ppf "@]@,}" ;
              if Hashtbl.length props <> Hashtbl.length props'
              || Hashtbl.fold (fun k (_, vb) acc ->
                  acc ||
                  (match Hashtbl.find props' k with
                   | (va, _) -> va <> vb
                   | exception Not_found -> true)) props false then begin
                fprintf ppf "@,@[<v 2>%g%% {" (100. *. tb /. maxt -. 0.001) ;
                Hashtbl.iter (fun k (_, v) -> print_prop ppf k v) props ;
                fprintf ppf "@]@,}"
              end
            end ;
            do_anim rest in
      fprintf ppf "@[<v 2>@keyframes anim-%s {" id ;
      let anim = compute_anim tml in
      let props = Hashtbl.create 100 in
      begin match anim with
        | [] -> ()
        | (_, _, fprops) :: _ ->
            Hashtbl.iter (fun k (v, _) -> Hashtbl.replace props k v) fprops end ;
      let rec find_common_props = function
        | [] -> ()
        | (_, _, fprops) :: rest ->
            Hashtbl.iter (fun k (va, vb) ->
                if va = vb then
                  match Hashtbl.find props k with
                  | v -> if v <> va then begin Hashtbl.remove props k end
                  | exception Not_found -> ())
              fprops ;
            find_common_props rest in
      let rec filter_common_props = function
        | [] -> ()
        | (_, _, fprops) :: rest ->
            Hashtbl.iter (fun k _ -> Hashtbl.remove fprops k) props ;
            filter_common_props rest in
      find_common_props anim ;
      filter_common_props anim ;
      do_anim anim ;
      fprintf ppf "@]@,}@,@[<v 2>#%s {@,animation: anim-%s %0.3gs linear 1; animation-fill-mode: forwards;" id id maxt ;
      Hashtbl.iter (fun k v -> print_prop ppf k v) props ;
      fprintf ppf  "@]@,}" in
    let rec print_shape ppf (shape, attrs) =
      let print_attrs ppf = function
        | None -> ()
        | Some (t, s) -> fprintf ppf " style='%a'" print_fixed_props (t, s) in
      match shape with
      | `Group l ->
          fprintf ppf "@[<v 2><g%a>@,%a@]@,</g>"
            print_attrs attrs
            (pp_print_list print_shape)
            (List.map (fun (shape, t, s) -> (shape, Some (t, s))) l)
      | `Path l ->
          let print_pathitem ppf = function
            | `Move_to (x, y) ->
                fprintf ppf "M %g %g" x y
            | `Line_to (x, y) ->
                fprintf ppf "L %g %g" x y
            | `Cubic_to ((xp, yp), (x, y)) ->
                fprintf ppf "C %g %g, %g %g" x y xp yp
            | `Quadratic_to  ((xp1, yp1), (xp2, yp2), (x, y)) ->
                fprintf ppf "C %g %g, %g %g, %g %g" x y xp1 yp1 xp2 yp2 in
          fprintf ppf "<path%a d='%a Z'></path>"
            print_attrs attrs (pp_print_list ~pp_sep:(fun p () -> fprintf p " ") print_pathitem) l
      | `Circle r ->
          fprintf ppf "<circle%a cx='0' cy='0' r='%g'/>"
            print_attrs attrs r
      | `Rect (w, h) ->
          fprintf ppf "<rect%a x='%g' y='%g' width='%g' height='%g'/>"
            print_attrs attrs (-. w /. 2.) (-. h /. 2.) w h in
    fprintf ppf "@[<v 2><svg width='%d' height='%d'>" w h ;
    List.iter (function
        | [] -> ()
        | [ _, img ] ->
            List.iter (fun ((_, shape), t, s) -> fprintf ppf "@,%a" print_shape (shape, Some (t, s))) img
        | tml ->
            let sm, maxt =
              List.fold_left (fun (sm, maxt) (t, img) ->
                  let sm = List.fold_left (fun sm ((id, shape), _, _) ->
                      match SM.find id sm with
                      | shape' ->
                          if shape <> shape' then failwith "duplicate ids" ;
                          sm
                      | exception Not_found ->
                          SM.add id shape sm)
                      sm img in
                  (sm, max maxt t))
                (SM.empty, -. infinity) tml in
            let print_shape ppf (id, shape) =
              fprintf ppf "<g id='%s'>@,%a@,</g>" id print_shape (shape, None) in
            let print_anim ppv (id, shape) =
              print_anim ppf (id, shape, tml, sm, maxt) in
            fprintf ppf "@,@[<v 2><style>@,%a@]@,</style>@,%a"
              (pp_print_list print_anim) (SM.bindings sm)
              (pp_print_list print_shape) (SM.bindings sm))
      tmls ;
    fprintf ppf "@]@,</svg>@."

  let () = Random.self_init ()

  let beveled_rect (w, h) b fill =
    let w, h = w /. 2., h /. 2. in
    `Group
      [ (`Rect (w *. 2., h *. 2.),
         identity,
         style ~fill ()) ;
        (`Path [ `Move_to (w, -.h) ;
                 `Line_to ((w -. b), -.(h -. b)) ;
                 `Line_to (-.(w -. b), -.(h -. b)) ;
                 `Line_to (-.(w -. b), (h -. b)) ;
                 `Line_to (-.w, h) ;
                 `Line_to (-.w, -.h) ],
         identity,
         style ~fill:"white" ~opacity:0.3 ()) ;
        (`Path [ `Move_to (-.w, h) ;
                 `Line_to (-.(w -. b), (h -. b)) ;
                 `Line_to ((w -. b), (h -. b)) ;
                 `Line_to ((w -. b), -.(h -. b)) ;
                 `Line_to (w, -.h) ;
                 `Line_to (w, h) ],
         identity,
         style ~fill:"black" ~opacity:0.3 ()) ]

  let to_svg anim = Format.asprintf "%a" print anim
end

let color =
  let t = Hashtbl.create 10 in
  let l = ref [ "#FC0" ; "#F90" ; "#690" ; "#C30" ; "#C60" ;
                "#06C" ; "#096" ; "#399" ; "#C66" ; "#990" ] in
  fun v ->
    try Hashtbl.find t v with Not_found ->
      let c = match !l with
        | [] -> Format.asprintf "#%06X" (Hashtbl.hash v land 0xFFFFFF)
        | h :: r -> l := r ; h in
      Hashtbl.add t v c ;
      c

let cells mx my =
  let rec cells acc x y =
    if y < 0 then acc
    else if x < 0 then cells acc mx (y - 1)
    else cells ((x, y) :: acc) (x - 1) y in
  cells [] mx my

 let grid gid cs bd d b w h cells =
  List.map (fun (x, y) ->
      (Format.asprintf "%s-bg-%d-%d" gid x y, `Rect (cs -. 2. *. b, cs -. 2. *. b)),
      Anim.transform ~move:(bd +. float x *. cs +. cs /. 2., bd +. float y *. cs +. cs /. 2.) (),
      Anim.style ~fill: "#ccc"())
    cells

let pieces gid cs bd d b arr cells =
  let open Anim in
  let w, h = Array.length arr.(0), Array.length arr in
  let rendered = Array.make_matrix h w false in
  List.fold_left (fun acc (x, y) ->
      if rendered.(y).(x) then acc
      else match arr.(y).(x) with
        | (X, _) -> acc
        | (k, n) as cell ->
            let kn = match k with X -> "X" | V -> "V" | S -> "S" | H -> "H" | C -> "C" in
            let uid = Format.asprintf "%s-p-%s-%d" gid kn n in
            rendered.(y).(x) <- true ;
            if x + 1 < w && y + 1 < h
               && arr.(y + 1).(x) = cell
               && arr.(y).(x + 1) = cell
               && arr.(y + 1).(x + 1) = cell then begin
              rendered.(y + 1).(x) <- true ;
              rendered.(y).(x + 1) <- true ;
              rendered.(y + 1).(x + 1) <- true ;
              ((uid, beveled_rect (2. *. cs -. d, 2. *. cs -. d) b (color cell)),
               transform ~move:(bd +. float x *. cs +. cs,bd +.  float y *. cs +. cs) (),
               style ()) :: acc
            end else if x + 1 < w && arr.(y).(x + 1) = cell then begin
              rendered.(y).(x + 1) <- true ;
              ((uid, beveled_rect (2. *. cs -. d, cs -. d) b (color cell)),
               transform ~move:(bd +. float x *. cs +. cs, bd +. float y *. cs +. cs /. 2.) (),
               style ()) :: acc
            end else if y + 1 < h && arr.(y + 1).(x) = cell then begin
              rendered.(y + 1).(x) <- true ;
              ((uid, beveled_rect (cs -. d, 2. *. cs -. d) b (color cell)),
               transform ~move:(bd +. float x *. cs +. cs /. 2., bd +. float y *. cs +. cs) (),
               style ()) :: acc
            end else
              ((uid, beveled_rect (cs -. d, cs -. d) b (color cell)),
               transform ~move:(bd +. float x *. cs +. cs /. 2., bd +. float y *. cs +. cs /. 2.) (),
               style ()) :: acc)
    [] cells

let base64_encode input =
  let code =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
     abcdefghijklmnopqrstuvwxyz\
     0123456789+/" in
  let to_char x = code.[x] in
  let length = String.length input in
  let words = (length + 2) / 3 in (* rounded up *)
  let padding_len = if length mod 3 = 0 then 0 else 3 - (length mod 3) in
  let output = Bytes.make (words * 4) '\000' in
  let get i = if i >= length then 0 else int_of_char input.[i] in
  for i = 0 to words - 1 do
    let x = get (3 * i + 0)
    and y = get (3 * i + 1)
    and z = get (3 * i + 2) in
    let n = (x lsl 16) lor (y lsl 8) lor z in
    let a = (n lsr 18) land 63
    and b = (n lsr 12) land 63
    and c = (n lsr 6) land 63
    and d = n land 63 in
    Bytes.set output (4 * i + 0) (to_char a);
    Bytes.set output (4 * i + 1) (to_char b);
    Bytes.set output (4 * i + 2) (to_char c);
    Bytes.set output (4 * i + 3) (to_char d);
  done;
  for i = 1 to padding_len do
    Bytes.set output (Bytes.length output - i) '=' ;
  done;
  Bytes.unsafe_to_string output

let display_board (arr : board) : unit =
  let open Anim in
  let cs = 30. and bd = 10. and d = 2. and b = 3. in
  let gid = "anim-" ^ string_of_int (int_of_float (Sys.time () *. 100.)) in
  let w, h = Array.length arr.(0), Array.length arr in
  let tw, th = float w *. cs +. 2. *. bd, float h *. cs +. 2. *. bd in
  let size = int_of_float (float w *. cs +. 2. *. bd),
             int_of_float (float h *. cs +. 2. *. bd) in
  let background_tml =
    [ 0., [ ("background", `Rect (tw -. 1., th -. 1.)),
            Anim.transform ~move:(tw /. 2., th /. 2.) (),
            Anim.style ~fill: "#eee" ~stroke: (1., "#ccc") () ] ] in
  let cells = cells (w - 1) (h - 1) in
  let grid_tml =
    [ 0., grid gid cs bd d b w h cells ] in
  let pieces_tml =
    [ 0., pieces gid cs bd d b arr cells ] in
  let animation =
    size, [ background_tml ; grid_tml ; pieces_tml ] in
  let svg = Anim.to_svg animation in
  let page = Format.asprintf
      "<html><title>Klotski Board</title>\
       <body style='padding: 5px; text-align: center; background: #ddd'>\
       \n%s\n\
       </body></html>" svg in
  let html = Format.asprintf
      "<div style='padding: 5px; margin: 0 30px 0 30px; text-align: center; background: #ddd'>\
       %s\
       click to open in a new window\
       </div>" svg in
  let output = Format.asprintf
      "<a href='data:text/html;base64,%s' target='_blank'>%s</a>"
      (base64_encode page) html in
  print_html output

let display_solution = function
  | [] -> invalid_arg "display_solution"
  | arr :: _ as arrs ->
      let open Anim in
      let cs = 30. and bd = 10. and d = 2. and b = 3. in
      let gid = "anim-" ^ string_of_int (int_of_float (Sys.time () *. 100.)) in
      let w, h = Array.length arr.(0), Array.length arr in
      let tw, th = float w *. cs +. 2. *. bd, float h *. cs +. 2. *. bd in
      let size = int_of_float (float w *. cs +. 2. *. bd),
                 int_of_float (float h *. cs +. 2. *. bd) in
      let background_tml =
        [ 0., [ ("background", `Rect (tw -. 1., th -. 1.)),
                Anim.transform ~move:(tw /. 2., th /. 2.) (),
                Anim.style ~fill: "#eee" ~stroke: (1., "#ccc") () ] ] in
      let grid_tml =
        let cells = cells (w - 1) (h - 1) in
        [ 0., grid gid cs bd d b w h cells ] in
      let pieces_tml =
        let arrs = match List.rev arrs with
          | [] -> []
          | last :: rest -> List.rev (last :: last :: rest) in
        List.mapi (fun i arr ->
            let w, h = Array.length arr.(0), Array.length arr in
            let cells = cells (w - 1) (h - 1) in
            float i *. 0.2, pieces gid cs bd d b arr cells) arrs in
      let animation =
        size, [ background_tml ; grid_tml ; pieces_tml ] in
      let svg = Anim.to_svg animation in
      let page = Format.asprintf
          "<html><title>Klotski Solution</title>\
           <body style='padding: 5px; text-align: center; background: #ddd'>\
           \n%s\n\
           </body></html>" svg in
      let html = Format.asprintf
          "<div style='padding: 5px; margin: 0 30px 0 30px; text-align: center; background: #ddd'>\
           %s\
           click to open in a new window\
           </div>" svg in
      let output = Format.asprintf
          "<a href='data:text/html;base64,%s' target='_blank'>%s</a>"
          (base64_encode page) html in
      print_html output

module Set : sig
  module type OrderedType = Set.OrderedType
  module type S = sig
    include Set.S
    val compare_elt : elt -> elt -> int
  end
  module Make: functor (Ord : OrderedType) -> S with type elt = Ord.t
end = struct
  module type OrderedType = Set.OrderedType
  module type S = sig
    include Set.S
    val compare_elt : elt -> elt -> int
  end
  module Make (Ord : OrderedType) = struct
    include Set.Make (Ord)
    let compare_elt = Ord.compare
  end
end

let array_get_counter = ref 0

module Array = struct
# 1 "array.ml"
  include Array
  let get a i =
    incr array_get_counter ;
    get a i
  let sub a ofs len =
    array_get_counter := !array_get_counter + len ;
    sub a ofs len
  let to_list a =
    array_get_counter := !array_get_counter + Array.length a ;
    to_list a
  let iter f a =
    let f x = incr array_get_counter ; f x in
    iter f a
  let map f a =
    let f x = incr array_get_counter ; f x in
    map f a
  let iteri f a =
    let f i x = incr array_get_counter ; f i x in
    iteri f a
  let mapi f a =
    let f i x = incr array_get_counter ; f i x in
    mapi f a
  let fold_left f acc a =
    let f acc e = incr array_get_counter ; f acc e in
    fold_left f acc a
  let fold_right f acc a =
    let f e acc = incr array_get_counter ; f e acc in
    fold_right f a acc
  let unsafe_get a i =
    incr array_get_counter ;
    unsafe_get a i
end

module ArrayLabels = struct
# 1 "arrayLabels.ml"
  include ArrayLabels
  let get a i =
    incr array_get_counter ;
    get a i
  let sub a ~pos ~len =
    array_get_counter := !array_get_counter + len ;
    sub a ~pos ~len
  let to_list a =
    array_get_counter := !array_get_counter + Array.length a ;
    to_list a
  let iter ~f a =
    let f x = incr array_get_counter ; f x in
    iter ~f a
  let map ~f a =
    let f x = incr array_get_counter ; f x in
    map ~f a
  let iteri ~f a =
    let f i x = incr array_get_counter ; f i x in
    iteri ~f a
  let mapi ~f a =
    let f i x = incr array_get_counter ; f i x in
    mapi ~f a
  let fold_left ~f acc a =
    let f acc e = incr array_get_counter ; f acc e in
    fold_left ~f acc a
  let fold_right ~f acc a =
    let f e acc = incr array_get_counter ; f e acc in
    fold_right ~f a acc
  let unsafe_get a i =
    incr array_get_counter ;
    unsafe_get a i
end

let graded_selection : int list option ref =
  ref None

let grade_only l =
  graded_selection := Some l
