type color_names = Blue | White | Red
type ball = color_names * int

let color ((c, _) : ball) = c
let index ((_, i) : ball) = i
