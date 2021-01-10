let copy = Array.copy
let init = Array.init
let iter = Array.iter
let length = Array.length
let make = Array.make
let map = Array.map

type index = int

type suffix = int

let prefix (h : int) (s : string) : string =
  assert (h <= String.length s);
  String.sub s 0 h

let suffix (s : string) (i : suffix) : string =
  let n = String.length s in
  assert (0 <= i && i <= n);
  String.sub s i (n-i)

let suffixes (s : string) (a : suffix array) : string array =
  map (suffix s) a
