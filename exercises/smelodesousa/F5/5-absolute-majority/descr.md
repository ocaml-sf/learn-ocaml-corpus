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

Imagine an electoral process in which candidates have an integer that identifies them (candidate 1, 2, 3, etc.).

In this configuration, a vote is the integer representing the candidate chosen by the voter. For example, a vote with $3$ is a vote towards candidate number $3$.

The votes are placed on an array. We pretend to know if there is an absolute majority in the election. In this case the candidate with the majority of the votes is elected.

# Objectives

1. Define the `majority : int array -> int` function that returns the integer belonging to the candidate that has the majority of the votes. If there is no majority, the exception `Not_found` is thrown.

There is an algorithm, called *MJRTY algorithm*, created by R. Boyer and J. Moore in 1980, which determines if there is a majority winner in $2\times N$ comparisons and only needs $3$ variables apart from the votes array. Another advantage is that it doesn't need to know how many candidates the election has.

Reference to the algorithm:

> Robert S. Boyer, J. Strother Moore.  *MJRTY - A Fast Majority Vote Algorithm*.  In R.S. Boyer (ed.), Automated Reasoning: Essays in Honor of Woody Bledsoe, Automated Reasoning Series, Kluwer Academic Publishers, Dordrecht, The Netherlands, 1991, pp. 105-117.  http://www.cs.utexas.edu/users/boyer/mjrty.ps.Z

Pseudocode for the algorithm:

```pseudocode
Prelude: The candidates are identified by integers.
         a is the array containing the votes  

let m be an integer and i a counter with a starting value of 0

for each element x of a
  if i = 0 then m := x and i := 1
  else if m = x then increment i
  else decrement i

if i = 0 then throw Not_found
if i > ⌊|a|/2⌋ then return m

reset i to 0
for each element x of a
  if x = m then increment i
  
if i > ⌊|a|/2⌋ then return m
else throw Not_found
```

The first cicle of this algorithm can be graphically represented in the following way:


![](https://i.imgur.com/TifapTj.png)

(taken from wikipedia)


We know the winner is decided if his advantage (value of $i$) is bigger than half of the votes. We also know that the voting hasn't missed any winners if that same value is $0$. In every other case, we can't conclude anything, meaning we need a new count, this time of the votes for the proposed winner.

In the image example, if the voting only had the first 7 votes, then the yellow candidate would have been excluded from being a winner in the second cycle.

2. Implement the `mjrty :int list -> int` function that implements this algorithm. <br />
   For example `mjrty [1; 1; 2; 1; 2; 3; 3; 2; 2; 2; 1; 2; 2; 3; 2; 2] = 2` <br />
   If the votes are inconclusive (no absolute majority), the function should throw a `Not_found` exception. <br />