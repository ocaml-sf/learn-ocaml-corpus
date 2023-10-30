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

Using an iterator (e.g. `fold_left`, `map`, `for_all`, `iter`, `exists`, `filter`,  etc.) define a function `max_seq : bool list -> int` which returns the length of the longest sequence of `true` from a list of booleans given in parameter.

For example, `max_seq [true; true; false ; true; false; true ; true; true; true; false; false; true]` returns $4$.