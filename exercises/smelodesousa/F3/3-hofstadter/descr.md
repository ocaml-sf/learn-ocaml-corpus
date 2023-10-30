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

In "Hofstadter, D. R. *[Gödel, Escher, Bach: An Eternal Golden Braid.](https://wikipedia.org/wiki/G%C3%B6del,_Escher,_Bach)* New York: Vintage Books, p. 137, 1989.",  Hofstadter defined various numerical sequences, two of which are the female and male sequences shown:


<center>$F(n) = {(1, if n=0), (n - M(F(n-1)),if n>0):}$</center>

<br />
<center>$M(n) = {(0, if n=0), (n - F(M(n-1)),if n>0):}$</center>

# Objective

Define the function `hfm : int  -> int*int` that, for a positive `int` parameter, returns the pair $(f(n), m(n))$.

In the case of an invalid argument or any error occurring, the exception `Failure "hfm"` is thrown.

As an example, `hfm 7 = (5,4)`.