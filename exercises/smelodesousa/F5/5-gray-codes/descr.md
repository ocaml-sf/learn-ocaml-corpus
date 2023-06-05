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

Gray codes allow for a good binary codification, in which only one bit changes between two consecutive elements.

To simplify the problem, let's only think about integers. In this case, the codification of $0$ is $0$, and $1$ is $1$. The codification of $17$ is $11001$, $18$ is $11011$ and $19$ is $11010$.

A simple way of generating gray codes from integer values up to length $n$ (for example $19$ has a length of $5$) is the *reflex-and-prefix* method.

The definition of the method is as follows:

```pseudocode
Base     reflex/prefix n.1     reflex/prefix n.2      reflex/prefix n.3       
0 0      0  *0     0  00       0  *00     0  000      0  *000    0  0000
1 1      1  *1     1  01       1  *01     1  001      1  *001    1  0001 
         reflex    prefix      2  *11     2  011      2  *011    2  0011
         2  *1     2  11       3  *10     3  010      3  *010    3  0010
         3  *0     3  10       reflex     prefix      4  *110    4  0110
                               4  *10     4  110      5  *111    5  0111
                               5  *11     5  111      6  *101    6  0101
                               6  *01     6  101      7  *100    7  0100
                               7  *00     7  100      reflex     prefix
                                                      8  *100    8  1100
                                                      9  *101    9  1101
                                                      10 *111    10 1111
                                                      11 *110    11 1110
                                                      12 *010    12 1010       
                                                      13 *011    13 1011
                                                      14 *001    14 1001 
                                                      15 *000    15 1000
```

# Goals

1. Define the function `gray_list int -> string list` that given an $n$ value calculates every gray code of length $n$. These codes are returned as a string list. <br />
If the argument is invalid, the exception `Invalid_argument "gray_list"` is thrown. <br />
For example `gray_list 2 = ["000";"001";"011";"010";"110";"111";"101";"100"]`.

2. Define the function `gray_code : int -> string` that returns the gray code of a certain $n$ integer parameter as a string.

3. Define the function `gray : int -> int` that calculates the gray codification of the positive integer passed as a parameter. <br />
If the argument is invalid, the exception `Invalid_argument "gray"` is thrown. <br />
For example `gray 9 = 13` (13 = 1101 in binary).

4. Define the function `de_gray : int -> int` that does the inverse operation, the decodification. <br />
If the argument is invalid, the exception `Invalid_argument "de_gray"` is thrown. <br />
For example, `de_gray 13 = de_gray 0b1101 = 9` (`0b1101` is 13 in binary notation).

**Note**: Feel free to define other functions to solve the problem.