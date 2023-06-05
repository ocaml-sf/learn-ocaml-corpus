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

Luigi’s new pizzeria has been the talk of the town in the past few weeks. Not only because it has the best pizzas you can find for miles, but also because of its crazy *all you can eat* policy.

You see, Luigi’s pizzas are enormous, and they are cut into very thin slices. And that’s not even the craziest part! Each slice has different ingredients and you can eat as many slices as you want. But there is one small caveat. You can only select adjacent slices and you have to eat them all! It is therefore very tricky for each client to select the best part of the pizza according to his taste.

> ------
>
> ![](https://i.imgur.com/4BJ0iBk.png)
>
> Figure 1: Selected slices must be adjacent. The section in grey has a score of 20.
>
> ------

You enter the restaurant and see that today’s special pizza has been cut into *N* slices. After attributing scores $(S_1, \ldots , S_N)$ to each one of the slices, you must devise an algorithm that selects the section of the pizza that yields the best value according to those scores. The value of a section of pizza is the sum of the scores of its slices. Notice that slice scores can be negative.

# Objectives

Define a function `score :int -> int list -> int ` that takes the number $N$ of slices in the pizza  and a list of $N$ integer that contains the integers ($S_i$) that represent the score you attributed to each slice. These values follow these constraints:

<table>
<tbody>
<tr>
<td align="center">$1 \leq N \leq 5 000$ </td>
<td align="center">    Number of slices.</td>
</tr>
<tr>
<td align="center">$−100 \leq S_i \leq 100$</td>
<td align="center">    Value of each slice.</td>
</tr>
</tbody>
</table>

The function returns an integer that is equal to the value of the best possible section of adjacent pizza slices. The smallest possible section would be a single slice.

For instance, 

- `score 4 [2;-2;3;-1]` returns `3`.
- `score 16 [-1;1;3;-8;3;-2;5;10;-2;-5;4;1;-7;13;-8;4]` returns `20`.
- `score 4 [-1;-2;-3;-4]` returns `-1`.