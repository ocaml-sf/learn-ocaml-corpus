We pretend to define the function `insertion: 'a -> 'a list -> 'a list list` which returns all the possible ways to insert an element `e` in a list `l`. Therefore, inserting `'e'` in the list `['a'; 'b'; 'c']` may result in the following lists:

- `['e'; 'a'; 'b'; 'c']`;
- `['a'; 'e'; 'b'; 'c']`;
- `['a'; 'b'; 'e'; 'c']`;
- `['a'; 'b'; 'c'; 'e']`.

Answer the following questions:

1. What is the expected result of `insertion 'e' []`?
2. What is the expected result of `insertion 'e' [c]`?
3. What is the expected result of `insertion 'e' [b;c]`?
4. What recursive pattern can we infer from these previous examples for the `insertion` function?<br>
Considering that pattern, define the function `insertion : char -> char list -> char list list`.