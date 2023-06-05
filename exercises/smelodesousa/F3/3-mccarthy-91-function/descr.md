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

In the 70s, John McCarthy, one of the minds behind artificial intelligence, in partnership with Zohar Manna and Amir Pnueli, tried to define a recursive function that could be used as a test case for various algorithms.

This function is the function $m : \mathbb{N} \to \mathbb{N}$ and can be defined in the following manner:

<center>$m(n) = {(n-10, if n>100), (m(m(n+11)), if n\leq 100):}$</center>

# Goals

1. Define the function `mccarthy : int -> int` that implements this recursive definition. As an example, `maccarthy 200 = 190` and `mccarthy 24 = 91`

2. Write an implementation of the function `f91` using the following definition:
<br />
  <center>$f91(n) ={(n-10, if n>100\), (91, if n\leq 100):}$</center><br />

3. Define a recursive function `mccarthy_is_f91 : int -> int -> bool` that verifies if, in the integer interval defined by the two `int` parameters, the functions return the same results, for example `mccarthy_is_f91 70 120 = mccarthy_is_f91 120 70 = true`.
