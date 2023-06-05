Define the following functions about lists:

1. The function `sum : int list -> int` which returns the sum of the integers contained in the received list.
For example, `sum [1;5;3]` returns `9`.<br />

2. The function `count_even : int list -> int` which returns the amount of even numbers present in the received list of integers. For example, `count_even [1;-6;3;17;4;80;-18]` returns `4`.

3. The boolean function `palindrome : int list -> bool `, which returns `true` if the list in parameter is a palindrome, or `false` otherwise. For example, `palindrome [1;5;3;5;1]` returns `true`.

4. The function `uppercase : char list -> char list` which transforms each character in the list that is a lowercase letter to an uppercase letter. For example, `uppercase ['a';'9';'T';'%';'z';'-']` returns `['A';'9';'T';%';'Z';'-']`.

5. The boolean function `is_sorted : int list -> (int -> int -> int) -> bool`, which returns true if the list in parameter is sorted according to sorting criteria indicated by the second parameter.<br />
Therefore, `is_sorted [1;3;7;9] compare` returns `true`, and `is_sorted [1;3;7;9] (fun a b -> compare b a)` returns `false`. Remember that the function `compare` from the OCaml standard library is defined as follows:<br />
```ocaml
compare a b = -1 if a < b
compare a b = 1 if a > b
compare a b = 0 if a = b
```

6. The function `remove_duplicate_sorted : int list -> int list` which removes duplicated elements from a list that is assumed to be sorted. For example, `remove_duplicate_sorted [1;1;2;2;3,5;5;6;7;8;8;8;9]` = `[1;2;3;5,6;7;8;9]`.

7. The function `remove_ duplicate : int list -> int list` which removes duplicated elements from a list. In this case, it is not assumed that the list is sorted. `remove_duplicate [9;1;7;6;6;7;8;1;2;6;2;3;5;5;1;9]` = `[1;2;3;5,6;7;8;9]`.<br />
Try, as much as possible, to use list operators from the OCaml module `List` (fold_left, map, for_all, iter, exists, filter, etc.).