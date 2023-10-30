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

Consider the following pattern composed of empty spaces and asterisks:

```pseudocode
*
* *
  *
* * * *
    *
    * *
      *
* * * * * * * *
        *
        * *
          *
        * * * *
            *
            * *
              *
```


# Objectives

Discover the construction rules that create the pattern and implement a recursive function `fractals : int -> unit` that, given an integer $n$, which is a power of $2$ contained in the interval $[1\ldots 100]$, produces this type of pattern in the `std_out` where $n$ is the number of asterisks the longest line will have.

For instance, `fractals 8` prints the pattern shown above.

In the case of an invalid argument, the exception `Invalid_argument "fractals"` is thrown.