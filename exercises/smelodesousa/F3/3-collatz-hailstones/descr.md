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

The Collatz conjecture (or Hailstones, or Syracuse) was thought of by the german mathematician Lothar Collatz as a challenge for the scientific community during an event at the University of Syracuse in 1928. The conjecture defines a number sequence (also referred to as trajectory or flight) that, starting on a natural integer, obeys the following principles:

- if *n* is even, then the successor of *n* in the sequence is *n* divided by 2
- if *n* is odd, then the successor of *n* in the sequence is multiplied by 3, plus 1.
- if the sequence reaches the number 1, then we stop.

To this day, nobody has found an initial value *n* such that the trajectory doesn't finish on the number $1$! (without the stopping condition, we would get an infinite loop starting on 1)

Examples:

<center>
$6 \to 3 \to 10 \to 5 \to 16 \to 8 \to 4 \to 2 \to 1$
</center>
<center>
$17 \to 52 \to 26 \to 13 \to 40 \to 20 \to 10 \to 5 \to 16 \to \cdots  \to 1$
</center>

# Objective

Your challenge is to write a recursive function `collatz : int  -> int list` in OCaml that, given a parameter *n*, returns the sequence of integers corresponding to the trajectory calculated from the value *n*. Obviously, this sequence stops when it reaches the value 1. For example:

`collatz 6 = [6; 3; 10; 5; 16; 8; 4; 2; 1]`