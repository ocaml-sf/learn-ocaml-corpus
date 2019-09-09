let nbmults = ref 0
let ( * ) x y = incr nbmults ; x * y
