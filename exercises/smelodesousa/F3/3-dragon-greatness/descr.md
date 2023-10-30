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

Imagine the following situation.

There is a paper ribbon that you will want to fold in half n times.

The initial configuration is (unfolded paper, profile view):

![](https://i.imgur.com/FrKFeHH.png)


If you fold it once and unfold it and let the angle make 90º, you get the following figure in profile:

![](https://i.imgur.com/FdNQ01N.png)

If you fold it twice and unfold it and let the angles obtained make 90º, you get the following figure in profile:

![](https://i.imgur.com/SKXenGJ.png)

If you fold it three times and unfold it and let the angles obtained make 90º, you get the following figure in profile:

![](https://i.imgur.com/ekQh8LV.png)


Very quickly, the paper folding exercise becomes tedious but visually curious (taken from (Wikipedia)):


![](https://i.imgur.com/4RtPGWP.gif)


The fractal obtained is called *dragon curve*.

The following image (also taken from Wikipedia) shows how to get a configuration from the previous one.

![](https://i.imgur.com/EY1Z8LP.png)

Let's encode these pictures with binary words. The principle is: "an angle to the left is 1" and "an angle to the right is 0". Thus:

- The tape with *zero* folds is represented by the empty word, $\epsilon$;
- The tape with *one* fold in the middle is encoded by the word $0$;
- The tape with *two* folds in the middle is encoded by the word $001$;
- The tape with *three* folds in the middle is encoded by the word $0010011$.

# Objectives

In this exercise, we are interested in answering two questions: what is the word obtained after n folds? what is the $m$-th letter of the word? To answer this, let's program.

1. Define a function `dragon_size: int -> bool list` which returns the size of the word after $n$ (given in parameter, a positive integer, possibly null) folds in the middle. For example, `dragon_size 4 = 15`. In case of an invalid argument, the exception `Invalid_argument "dragon_size"` is thrown.

2. Define a function `dragon: int -> bool list` which returns the list of booleans that form the dragon curve word for n (in parameter) folds. For example, `dragon 3 = [false; false; true; false; false; true; true]`. In case of an invalid argument, the exception `Invalid_argument "dragon"` is thrown.

3. Define a function `dragon_bit : int -> bool` which for a nonzero positive integer $n$ (in parameter) returns the $n$-th bit of the dragon sequence. For example, `dragon_bit 11 = true`. In case of an invalid argument, the exception `Invalid_argument "dragon_bit"` is thrown. 