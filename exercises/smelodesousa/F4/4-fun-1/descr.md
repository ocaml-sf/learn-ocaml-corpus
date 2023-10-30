Provide the type of the following expressions:

1. `let x = read_int () in let y = read_float () in (x+1,y)`<br />
2. `let f a b c = b (c + a)`<br />
3. `let f a b c = b c`<br />
4. `let f a (b: int -> int) c = a (b c)`<br />
5. `let f a b c = a  (b c)`<br />
6. `let f a b c = c (b a)`<br />
7. `let o = ref (List.tl [1])`<br />
8. `let x = ref []`<br />
9. `let x = [ ( + ) 3 ; succ ; (fun x y -> List.length  x+ y) [1;5] ]`<br />
10. `let x y = y 1 in x (fun z -> z + 1) `<br />
11. `let f x y = function z -> y (List.map x z)`<br /><br />

**Note:** If necessary, use the type `'_weak1`. It should be used as follows:<br />
  `type answer = _weak1`, which means, without the apostrophe.