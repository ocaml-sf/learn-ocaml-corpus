For each expression, specify their type:

1. `let f1 x = !x` 
2. `let f2 g x y = if g x then y x else g` 
3. `let f3 h = let x = ref true in if h x then x:= false; !x`