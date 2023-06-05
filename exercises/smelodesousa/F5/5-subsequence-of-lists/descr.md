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

Implement a function `subseq : 'a list -> 'a list -> bool` that determines if a list `w1` is a subsequence of another list `w2`. A list `l1` is a subsequence of a list `l2` if we can obtain `l1` from `l2` by removing $0$ or more elements from the latter. For example, `[4;7;5;1]` is a subsequence of `[4;5;4;6;2;7;5;6;8;1;0]`.
​