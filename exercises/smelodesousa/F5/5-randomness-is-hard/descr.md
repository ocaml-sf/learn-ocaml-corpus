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

Our next problem revolves around the following question:

Given an array `v` of size `n` already initialized, how do we rearrange its elements in a simple way (i.e., not _very_ inefficiently) so that we end up with a shuffled version of the initial array?

In other words, how do we find a permutation of its elements that seems random?

The problem raised by this question seems simple, but in reality, it is not. Determining a permutation with good random properties in a simple way is not a straightforward problem to solve.
 
In 1938, Ronald Fisher and Frank Yates, in the book _Statistical tables for biological, agricultural and medical research_, described a method that was later studied and brought to light by Donald Knuth himself...

This method became known as *Knuth shuffle* or _Fisher-Yates-Knuth shuffle_.

```ocaml
(* To shuffle an array v with n elements (indexes 0...n-1), do the following: *)
  for i = n - 1 downto 1 do
       let j = random int where 0 <= j <= i
       swap v[j] and v[i]
```

The essential property of this method is that every possible permutation has the same probability of being returned by the algorithm, including the original permutation.

# Objective

Implement the function `knuth_shuffle: 'a array -> 'a array` that incorporates the algorithm presented above. Note that the received argument is the array we want to shuffle. The function `Random.int` from the OCaml module `Random` might be handy for this exercise (ref. https://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html).

> Random.int : int -> int
> 
> Random.int `bound` returns a random integer between $0$ (inclusive) 
>   and `bound` (exclusive). `bound` must be greater than $0$ and less than $2^30$.

Note that it is *not expected* that you use functions such as `Random.init` or `Random.self_init`.