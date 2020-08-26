let copy = Array.copy
let init = Array.init
let iter = Array.iter
let length = Array.length
let make = Array.make
let map = Array.map

type index = int

type suffix = int

let prefix (h : int) (s : string) : string =
  let n = String.length s in
  if n <= h then s else String.sub s 0 h

let suffix (s : string) (i : suffix) : string =
  let n = String.length s in
  String.sub s i (n-i)

let suffixes (s : string) (a : suffix array) : string array =
  map (suffix s) a
