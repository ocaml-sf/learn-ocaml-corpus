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

Implement a function `digits : int -> int -> int*int*int` which receives an integer $n$ and a digit $i$, and returns the number of times the digit $i$ appears in $n$, the sum of the digits of $n$ and finally the number of digits of $n$.

For example, `digits 1073741823 3` would return the tuple `(2,36,10)`.
