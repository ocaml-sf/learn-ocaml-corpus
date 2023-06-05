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

1. Define a function `distance : (float * float) -> (float * float) -> float` that calculates the distance between two given points in parameters.

2. Define a function `area: float -> float` that calculates the area of a circle whose radius is given as a parameter.

3. Define a function `sin2x : float -> float` that, given a floating parameter x, calculates the following expression:

<center>
$\frac{2 \times tanx}{1+tan^2x}$
</center>
