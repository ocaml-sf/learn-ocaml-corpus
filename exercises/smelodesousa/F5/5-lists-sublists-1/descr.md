We pretend to define the function `sublist: 'a list -> 'a list list` which returns the list containing all sublists of a list `l`, with the elements presented in the order of the original list `l`.

For example, `sublist ['a'; 'b'; 'c']` = `[[];['c'];['b'];['b'; 'c'];['a'];['a'; 'c'];['a'; 'b'];['a'; 'b'; 'c']]`. Note that `['a'; 'c']` is a sublist of `['a'; 'b'; 'c']`, but `['c'; 'a']` is not.

1. What is the expected result of `sublist []`?
2. What is the expected result of `sublist ['c']`?
3. What is the expected result of `sublist ['b'; 'c']`?
4. What recursive pattern can we infer from these previous examples for the `sublist` function?<br>
Considering that pattern, define the function `sublist : char list -> char list list`.