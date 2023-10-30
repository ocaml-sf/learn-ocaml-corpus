Let's now define the function `permutation` which returns the list containing all permutations of the parameter list `l`. For example, `permutation ['a'; 'b'; 'c'] = [['a'; 'b'; 'c']; ['b'; 'a'; 'c']; ['b'; 'c'; 'a']; ['a'; 'c'; 'b']; ['c'; 'a'; 'b']; ['c'; 'b'; 'a']]`.

1. What is the expected result of `permutation []`?
2. What is the expected result of `permutation ['c']`?
3. What is the expected result of `permutation ['b'; 'c']`?
4. What recursive pattern can we infer from these previous examples for the `permutation` function?<br>
Considering that pattern, define the function `permutation : char list -> char list list`.