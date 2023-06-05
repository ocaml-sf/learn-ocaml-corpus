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

The famous integer sequence known as the *_Catalan Number_* (from the Belgian mathematician Eugène Charles Catalan, 1814–1894) is defined by:

<center>$
Catalan(n) = {
(1, if n=0 vv n=1),
(\sum_{(p,q)\ such\ t h at\ p+q=n-1} Catalan(p)*Catalan(q), if n>1
):}
$</center>

This sequence is seen in countless combinatorial problems. 

# Objective

Write a function `catalan : int -> int` that takes in a natural integer $n$ and recursively calculates the value of $Catalan(n)$.

In the case of an invalid argument, the `Invalid_argument "catalan"` exception is thrown.

As an example, `catalan 6 = 132`.