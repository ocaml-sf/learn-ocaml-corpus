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

Wilhelm Friedrich Ackermann, a German mathematician, contributed to the study of computable functions by presenting a function with unique properties: a total computable function that escaped, in the time of its discovery, the classification that was thought to be that of functions with these characteristics.

Partly because the function... has an explosive behavior!

The initially proposed function had 3 parameters, but the one that was recorded for history includes two parameters and is defined as follows:

<center>
$A(0,n) = n + 1$ <br />
$A(m+1,0) = A(m,1)$ <br />
$A(m+1,n+1) = A(m,A(m+1,n))$
</center>

With paper and pencil, determine the result of A(4,4).

If you read this sentence, know that A(4,2) results in a number with more than 19,000 digits ($2^{2^{2^{2^{2}}}}-3$) and that the previous paragraph is the result of a bad joke. It is not expected that you solve it! 

# Goal

Define the recursive function `ackermann int -> int -> int`, which replicates this function.

For example, `ackermann 3 4 = 125`.