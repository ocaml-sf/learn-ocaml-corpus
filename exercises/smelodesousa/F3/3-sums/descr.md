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

Consider the following mathematical expression:

<center>$underset(i=0)(sum^n) 3^i$</center>

<ol type="1">
  <li>Define the function <code>sum3 : int -> int</code> such that <code>sum3 n</code> returns the value $underset(i=0)(sum^n) 3^i$.</li>
  <li>Provide a tail recursive version <code>sum3_tr : int -> int -> int</code>.</li>
</ol> 