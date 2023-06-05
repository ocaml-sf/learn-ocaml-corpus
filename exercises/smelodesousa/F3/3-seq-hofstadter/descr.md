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

Consider $r$ and $s$ to be two natural positive integers, where $s\geq 2$ and $r<s$. The Hofstadter-Huber sequence of family $(r,s)$ is the sequence determined by the following formula:

<center>$Q_{r,s}(n) = 
{(1, if\ 1 \leq n \leq s),
(Q_{r,s} (n - Q_{r,s} (n-r)) + Q_{r,s} (n - Q_{r,s} (n - s)), if\ n>s
):}$</center>

where $n$ is a positive integer.

<p>Although, this family of values suffers from some irregularities. In particular, when the values $Q_{r,s}(n)$ are not defined (i.e. when $n -  Q_{r,s}(n-r) < 1$  or  $n -  Q_{r,s}(n-s) < 1$) or whenever any other established condition about $r$ and $s$ isn't respected, we say that the sequence <emph>dies</emph>.</p>

# Objective

<p>Implement the function <code>hhq : int -> int -> int -> int</code> so that <code>hhq r s n</code> determines the value of $Q_{r,s}(n)$.</p>

In case of an invalid argument or an irregular situation, the exception `Failure "hhq"` is thrown.
Thus, `hhq 1 4 12 = 7`.