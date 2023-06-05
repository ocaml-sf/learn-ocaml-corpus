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

In this exercise, we pretend to sort lists or arrays of integers with the help of `List.sort` or `Array.sort`, according to the provided criteria:

1. Implement a function `sort1 : int array -> int array` that sorts the receiving array in descending order using the function `Array.sort`.

2. Implement a function `sort2 : int list -> int list` that sorts the receiving list using the function `List.sort` and the following criteria:

    - Odd integers first (we are considering them to be lower than even numbers), even integers next;
    - Odd numbers must be in descending order;
    - Even numbers must be in ascending order.

3. Implement a function `sort3 : int array -> int array` that sorts the receiving array using the last exercise's criteria, but this time with the help of `Array.sort`.

4. Implement a function `sort4 : int list -> int list` that sorts the receiving list's integers by the lexicographic order of their values read backward. For example, let us consider $19$ and $111$. By comparing them from right to left, the $9$ from $19$ is greater than the $1$ from $111$. Consequently, we get that $19 > 111$ by comparing them in this particular order.

   Thus, `sort4 [121;17;191;32;19;91]` returns `[121;91;191;32;17;19]`.