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

In this exercise, we'll simulate a lotto prize draw.

1. Define the `make_grid` function that, given an integer `n`, returns a grid (matrix) of size $n\times n$ initialized with `false` values.

2. Specify the type of the grid that the function defined above creates.

3. Define the `grid` global variable using the function above and with size $7 \times 7$.

4. Define the `fill: int list -> grids` function that, given a list with $7$ distinct integers comprised between $1$ and $49$, creates and fills a grid that is returned in the end. On the created grid, a position from the prize draw (from the list parameter) has a `true` value. It is important to note that the grid positions correspond to the number of the prize draw. <br/>
For example, the 7th position on the grid, grids.(0).(6), is the number $7$ of the prize draw. 

5. Define the `prize_draw : grids -> int list -> int -> (int list * bool)` that given a draw (list of $6$ integers, plus another integer -- the complementary) returns the correct guesses. 

For example if the draw is $1$, $5$, $23$, $30$, $31$ and $45$ and the complementary number is $17$, and if the `grid` with the complementary is $1$, $17$ and $30$, then the answer to `prize_draw grid [1; 5; 23; 30; 31; 45] 17` should be `([1; 30], true)`, meaning "you got $1$ and $30$ right, and were also right on the complementary".