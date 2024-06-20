module MultiSet : MultiSet_S = struct
  type 'a t = Empty | Not_empty of (int * ('a, int) Hashtbl.t)
  let occurrences t v =
    match t with
    | Empty -> 0
    | Not_empty (_, h) -> try Hashtbl.find h v with Not_found -> 0
  let empty = Empty
  let insert t v =
    match t with
    | Empty ->
        let h = Hashtbl.create 10 in
        Hashtbl.add h v 1 ;
        Not_empty (1, h)
    | Not_empty (n, h) ->
        let h = Hashtbl.copy h in
        begin match Hashtbl.find h v with
          | o -> Hashtbl.replace h v (o + 1)
          | exception Not_found -> Hashtbl.add h v 1 end ;
        Not_empty (n + 1, h)
  let remove t v =
    match t with
    | Empty -> Empty
    | Not_empty (1, h) ->
        if Hashtbl.mem h v then Empty else t
    | Not_empty (n, h) ->
        match Hashtbl.find h v with
        | 1 ->
            let h = Hashtbl.copy h in
            Hashtbl.remove h v ;
            Not_empty (n - 1, h)
        | o ->
            let h = Hashtbl.copy h in
            Hashtbl.replace h v (o - 1) ;
            Not_empty (n - 1, h)
        | exception Not_found -> t
end

let letters word =
  let s = ref MultiSet.empty in
  String.iter (fun c -> s := MultiSet.insert !s c) word;
  !s

let anagram word1 word2 =
  String.(length word1 = length word2) && begin
    let s = ref (letters word1) in
    String.iter (fun c -> s := MultiSet.remove !s c) word2;
    !s = MultiSet.empty
  end
