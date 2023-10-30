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

Let `v` be an *ascending-ordered* vector of integers of size `n`, and `x` an integer.

We will define an *efficient* search function that takes advantage of vector sorting.

Consider two indices `i`, `j` of vector `v` such that $i\leq j$. We will look for x in the vector `v` between the indices i and j, (notation $v[i \ldots j]$)

- If $i=j$ then $x\in v[i\ldots j]$ if and only if $x = v[i]$.
Otherwise:

- If $v[i] > x$ then we know that $x$ is not in the $[i,j]$ segment of the vector $v$. In the best case, it is somewhere in the $[0,i[$ segment of the $v$ vector.

- If $x < v[j]$ then we know that $x$ is not in the $[i,j]$ segment of the vector $v$. In the best case, it is somewhere in the $]j,n[$ segment of the $v$ vector.

- If $v[i] \leq x \leq v[j]$ then we know that $x$ is possibly in the segment $[i,j]$ of the vector $v$. What we know for sure is that x is neither in $v[0 \ldots i-1]$ nor $v[j \ldots n]$. Consequently, the search must be focused on the segment $[i,j]$.

  We have $i<j$, so let $m$ be the index "in the middle", in other words, the largest integer that is less than or equal to the mean of i and j.

  - If $x = v[m]$, we find $x$.
  - If $x > v[m]$, then the segment $v[i\ldots m]$ of the vector $v$ does not contain x. Possibly it will be in $v[m+1\ldots j]$.
  - If $x < v[m]$, then the segment $v[m \ldots j]$ of the vector $v$ does not contain x. Possibly it will be in $v[i\ldots m-1]$.

  This method is called *binary search* or *dichotomous search*. 

# Objectives

1. Define a recursive binary search function `binsearch_aux : 'a -> 'a array -> int -> int -> int` . `binsearch_aux x v low high` search for the value `x` in the ordered vector `v` between indexes `low` and `high`. This function returns the index where the `x` value lies in `v` (in the `low..high` range), or else the exception `Not_found` is thrown in any other situation. 

    For instance, `binsearch_aux 12 [|1;2;5;7;12;16;23;33;78|] 2 6` returns `4`.

2. Define a `binsearch function: 'a -> 'a array -> int` that searches for the value `x` in the entire sorted array `v`. This function returns the index where the value `x` is in `v` or the exception `Not_found` is thrown. It is recommended to use the function from the previous exercise.

    Note that this search divides the search space by two each time the search is refined. This feature greatly improves response times. The worst case is when the element to be searched is not present in the vector. Nevertheless, the number of comparisons performed by the algorithm never exceeds the order of $log_2(n)$ where $n$ is the size of the vector.