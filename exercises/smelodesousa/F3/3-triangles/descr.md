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

Consider the following sequence of equilateral triangles:

![](https://t1.daumcdn.net/cfile/tistory/2272113A566FC05815)

If we list the length of the sides of the triangles in this sequence, we have:

<center>$1, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 16, \cdots$</center>

# Goal

Define a function `triangles  : int -> int` which returns the nth element of this sequence (starting at index 0).

For example, `triangles 0 = 1`, `triangles 3 = 2` and `triangles 9 = 9` .
