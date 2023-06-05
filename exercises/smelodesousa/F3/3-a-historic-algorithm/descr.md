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

In this exercise, we will implement a historic algorithm formulated by Euclid himself in 300 BC.
In particular, we will work with the recursive version:

<center>
$gcd(a, b) ={(a,if\ b=0), (gcd(b,mod(a,b)), otherwise\):}$
</center>


# Objectives

Define a function `euclid : int -> int -> int` that, given two non-negative integers, determines their greatest common divisor based on the algorithm above. Thus, `euclid 36 45 = 9`. In case of an invalid argument, the exception `Invalid_argument "euclid"` is thrown.
