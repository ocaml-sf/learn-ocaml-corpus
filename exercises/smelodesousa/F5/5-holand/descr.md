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

This programming problem was originally proposed by [Edsger Dijkstra](https://en.wikipedia.org/wiki/Edsger_Dijkstra) to illustrate and highlight some expected properties of a sorting algorithm, such as stability.

Given an arbitrary sequence of 3 colored balls of arbitrary length. How to sort this sequence so that the sequence is ordered (the blue balls first, followed by the white balls, and finally the red balls)? Moreover, it is intended that the original order of balls of the same color is respected! 

For instance, in the unordered sequence, if a particular blue ball is in a more left-handed position than another blue ball, then in the ordered sequence, this order holds. It remains in a more left-handed position than the other, and the ordering is said to be *stable*.

The algorithm proposed by Dijkstra for this problem shows that it is possible to sort a collection of $n$ colored objects using a *linear* number of color comparisons. Although, classical sorting algorithms (the family of sorting algorithms that do not use the information particular about objects for sorting, for example, here the knowledge that there are only three colors, blue, white, and red) needs on average (and in the worst case in the best algorithms) a greater number of comparisons, on the order of $n .log(n)$

We will assume the OCaml type to represent the colors and the following utility functions:

```ocaml
type color_names = Blue | White | Red
type ball = (color_names*int)
let color ((c,_) : ball) = c 
let index ((_,i) : ball) = i
```

Then, we propose this pseudocode for the desired sorting. Note that this pseudocode *purposely has "subtle" errors*!

```pseudocode
input: a: vector of ball elements

b := 0
i := 0
r := length of a

When i < r do
 if color a[i] = Blue then 
    swap the values of a[b] e a[i]
    increment b 
 else if cor a[i] = White then 
    increment i
 else // color a[i] = Red
    increment r
    swap the values of a[r] e a[i]
 end do    
```

# Objetive

After executing this pseudocode on paper and correcting any errors it contains, define the function `dutch_flag : ball array -> ball array` that sorts the parameter vector using this algorithm. Assigning an index to each color using the pair `(color_names * int)` enables us to determine whether the suggested approach is stable.

Therefore: `dutch_flag [|(Red,0);(White,1);(Blue,2);(Red,3);(Blue,4);(White,5);(Blue,6);(Red,7);(White,8);(Blue,9)|] = [(Blue,2);(Blue,4);(Blue,6);(Blue,9);(White,1);(White,5);(White,8);(Red,0);(Red,3);(Red,7)|] `