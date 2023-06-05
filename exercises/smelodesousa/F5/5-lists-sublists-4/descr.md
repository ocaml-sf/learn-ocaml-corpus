<script>
MathJax = {
  loader: {load: ['input/asciimath', 'output/chtml']},
  asciimath: {
    delimiters: [['$','$'], ['`','`']]
  }
}
</script>

<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
<script type="text/javascript" id="MathJax-script" async
  src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/startup.js"></script>

Finally, let's define `subbag l`, which calculates the list of all of the permutations of every sublist of `l`. For example: <br />
`subbag ['a'; 'b'; 'c'] = [[]; ['a'] ; ['b']; ['c'] ; ['a'; 'b'] ; ['a'; 'c'] ; ['b'; 'a'] ; ['b'; 'c'] ; ['c'; 'a'] ; ['c'; 'b'] ; ['a'; 'b'; 'c']; ['a'; 'c'; 'b'] ; ['b'; 'a'; 'c'] ; ['b'; 'c'; 'a']; ['c'; 'b'; 'a'] ; ['c'; 'a'; 'b']]`.

This function calculates something more "explosive" than the group of all subsets of a certain set (or list), given that the order is relevant <br />
(`['a'; 'b']` $ne$ `['b'; 'a']`).

1. Define the function `subbag : char list -> char list list`, with the functions defined in the previous exercises in mind.

Isn't there a way of defining the function without using the previous ones? To achieve that, use the incremental methodology previously recommended to extract a programmable pattern. We propose this challenge for your curiosity.