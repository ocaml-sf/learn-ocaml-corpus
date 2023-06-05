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

Here we will consider a classic combinatorial (counting) problem.

Given a value $n$, imagine a mountain landscape inscribed in a grid $(0,0)$ through $(2n,0)$.

In our scenario, a landscape consists exclusively of mountains whose slope is a diagonal _one by one_ ascending or descending, starting at $(0,0)$ and ends at $(2n,0)$. 

Mountain profiles that descend below the $x$-axis are also not considered.

The unsolved problem is: given $n$ (which defines the landscape size) and the number $k$ of peaks ($k$ always less than or equal to $n$), how many valid mountain profiles are there?

For example, for $n=4$ and $k=3$, the answer is $6$. Graphically we can visualize the solutions as follows:

![](https://i.imgur.com/lIZsgHT.png)


We also give two limit values for $N=4$, which are $k=1$ and $k=4$

![](https://i.imgur.com/NwVI8wo.png)


We also assume that $1\leq n \leq 30$ and $1\leq k \leq n$.

# Objective

Define a function `mountains: int -> int -> int option` that calculates precisely the number of possible profiles. Therefore, the result of `mountains 4 3` is `Some 6`. When the rules about `n` and `k` are not satisfied, or for some reason the value cannot be calculated, the function returns `None`. Thus, the result `mountain 3 4` is `None`.

Hint: consider the initial cases ($n=1$ and $k=1$, followed by $n=2$ and $k=1$ or $k=2$, etc.) and check if a pattern emerges as you calculate the next cases.