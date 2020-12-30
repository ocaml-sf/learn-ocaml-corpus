module CharHashedType =
struct (* replace this structure with your implementation *) end

module CharHashtbl =
struct (* replace this structure with your implementation *) end

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let empty () =
    "Replace this string with your implementation." ;;

  let lookup trie w =
    "Replace this string with your implementation." ;;

  let insert trie w v =
    "Replace this string with your implementation." ;;

end
