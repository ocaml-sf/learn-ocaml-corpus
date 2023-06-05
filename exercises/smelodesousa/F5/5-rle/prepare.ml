type rle_contents = 
  One of int 
  | Many of (int*int)

type choice =
  | A | B | C | D | To_Answer of string