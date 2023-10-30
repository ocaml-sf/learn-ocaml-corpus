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

The maximum sublist problem consists of, given a list of integers, finding the contiguous sublist with the largest sum of all existing contiguous sublists.

For example, looking at the list `[ -3; 6; -3; 4; -1; 2; 2; -5; 4 ]`, the sublist with the largest sum is `[6; -3; 4; -1; 2; 2]`, which sum is 10. This sublist might not be unique, however. The list `[ -3; 7; -11; 4; -1; 2; 2; -5; 4 ]` has two sublists with a sum of 7. The maximum sublist might not be unique, but the largest sum is.

To efficiently solve this problem, in 1984, Jay Kadane (from Carnegie Mellon University) presented an algorithm that solves this problem by going through the list just once.

The algorithm is defined recursively in the following manner:
- If the list l is empty, then the sum is $0$.
- If the list l is $[v_1; v_2; \ldots ; v_ {i−1}; v_i; \ldots ; v_n]$ and $m$ is the largest possible sum for the sublist that ends in $i-1$, then the biggest sum of $[v_1; v_2; \ldots ; v_ {i−1}; v_i]$ is $max(v_i, m+v_i)$.

    Note: The empty list will be considered the leftmost sublist of any list. Its sum is also 0.

# Objectives

1. Implement a function `max_kadane : int list -> int` that incorporates the previously shown Kadene's algorithm and returns the largest possible sum of a contiguous sublist from the original list. If necessary, you may use an extra list to memoize the previously found largest sums.

2. Implement a function `kadane : int list -> int list` that returns the sublist which has the largest sum. In case there are multiple sublists, you may return the leftmost one.