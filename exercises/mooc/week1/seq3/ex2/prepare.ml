let nbconcs = ref 0
let ( ^ ) x y = incr nbconcs ; x ^ y
let word = [| "ook" ; "plop" ; "burp" ; "blah" ; "bark" |].(Random.int 5)
