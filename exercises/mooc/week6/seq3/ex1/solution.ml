module CharHashedType : Hashtbl.HashedType with type t = char = struct
  type t = char
  let hash c = Char.code c
  let equal c1 c2 = (c1 = c2)
end

module CharHashtbl = Hashtbl.Make (CharHashedType)

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let children_from_char cmap c =
    try Some (CharHashtbl.find cmap c) with Not_found -> None

  let update_children cmap c t =
    let cmap = CharHashtbl.copy cmap in
    CharHashtbl.replace cmap c t;
    cmap

  let no_child () = CharHashtbl.create 1

  let empty () = Trie (None, no_child ())

  let lookup trie w =
    let rec aux i (Trie (label, cmap)) =
      if i = String.length w then label else
	match children_from_char cmap w.[i] with
	  | None -> None
	  | Some t -> aux (i + 1) t
    in
    aux 0 trie

  let insert trie w v =
    let rec aux i (Trie (label, cmap)) =
      if i = String.length w then Trie (Some v, cmap) else
	match children_from_char cmap w.[i] with
	| None ->
            Trie (label, update_children cmap w.[i] (aux (i + 1) (Trie (None, no_child ()))))
	| Some t ->
	    Trie (label, update_children cmap w.[i] (aux (i + 1) t))
    in
    aux 0 trie
end
