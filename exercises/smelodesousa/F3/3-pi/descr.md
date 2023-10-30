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

_Wallis_'s formula provides an approximation of $\Pi$ defined in the following manner:

<center>$\frac{\Pi}{2} = \frac{2\times 2}{1 \times 3} \times \frac{4\times
  4}{3 \times 5} \times \frac{6\times 6}{5 \times 7} \cdots$</center>

- Provide a recursive definition of this approximation by creating a function $f$ that receives an index $k\geq 1$ so that if $k=1$, the function calculates the following formula:

<center>$
2 \times \frac {2\times 2} {1 \times 3} 
$</center>

&emsp;&emsp;&emsp;, if $k=2$, then $f(2)$ determines

<center>$
2 \times
\frac{2\times 2}{1 \times 3} \times \frac{4\times
  4}{3 \times 5}
$</center>

&emsp;&emsp;&emsp;, etc.

# Objective

Given the definition above, implement this function in OCaml in the form of the function `approximate_pi : int -> float`, that given the index $k$, returns the result of $f(k)$.

Consider that $0 < k \leq 10000$. If it is not possible to determine $f(k)$, `approximate_pi` throws the exception `Failure "approximate_pi"`.

Example:  `approximate_pi 5 = 3.00217595455690622`