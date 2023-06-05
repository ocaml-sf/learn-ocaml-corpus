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

# Introduction

Consider the following _tribonacci_ function over natural integers:

<center>$"tribonacci"(n) = {
(1, if\ n=0),
(1, if\ n=1),
(1, if\ n=2),
(\Sigma_{i=1}^3 "tribonacci"\ (n-i), if\ n>2)
:}$</center>


# Objectives

1. Implement the recursive function `tribonacci : int -> int` that determines the tribonacci value for any given number based on the definition above. In the case of an invalid argument, the exception `Invalid_argument "tribonacci"` is thrown.

2. Implement the iterative version `tribonacci_iter : int -> int` with loops and references to determine the tribonacci value for any given number.

3. Implement the tail recursive function `tribonacci_tail : int -> int -> int -> int`. Assume that when `tribonacci_tail n a b c` is called, the values of a, b and c are 1.

Example:  `tribonacci_tail 6 1 1 1 = 17`