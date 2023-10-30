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
We can represent a polynomial $P$ of degree $n$ with a list $p$ of real numbers, where its $i$th element represents the coefficient associated with the exponent of degree $i$.

Thus, the polynomial $3x^4+5x^2+1$, for example, is represented by the list `[3;0;5;0;1]` (or `[1;0;5;0;3]`, if we prefer to list the polynomial from the smallest degree to the highest). In this exercise, we will assume that the highest degree is always at the left.

# Objectives

1. Choose the option that best defines the type `polynomial` as a pair of an integer number that represents the highest degree of the polynomial and a list of real numbers. Using the example from above, its value of type `polynomial` would be `(4, [3.; 0.; 5.; 0.; 1.])`. Note that the integer representing the highest degree is a non-negative integer. In other words, the list cannot be empty, otherwise, a polynomial wouldn't exist.

    A) `type polynomial = { degree : int; polynomial : float list }` <br />
    B) `type polynomial = (int * (float list))`<br />
    C) `type polynomial = ((float list) * int)`<br />
    D) `type polynomial = { polynomial : float list; degree : int }`<br />
    E) `type polynomial = (float * (int list))`<br />
    F) `type polynomial = { polynomial : int list; degree : float }`<br /><br />

    (Note: If you believe the correct option is `A`, then you should answer as follows: `let answer = A`)

2. Implement a function `horner : float -> polynomial -> float` that, given a real number $x$, determines $P(x)$ by using Horner's method, i.e.

    <center>
    $P_n(x)=(\cdots((a_n x + a_{n-1})x + a_{n-2})x + \cdots + a_1)x + a_0$
    </center>
    
    Thus, `horner 3.0 (4,[3.; 0.; 5.; 0.; 1.])` returns `289.0`. In case of an invalid argument, the exception `Invalid_argument "horner"` may be thrown.

3. Implement a function `derivative : polynomial -> polynomial` that, given a polynomial $P(x)$ in the form of a list, determines its respective derivative, which is also a polynomial. In case of an invalid argument, the exception `Invalid_argument "horner"` may be thrown.

    For example, `derivative (4,[3.; 0.; 5.; 0.; 1.])` returns `(3,[12.; 0.; 10.; 0.])`.

4. Create a tail recursive version `derivative_tr : polynomial -> polynomial -> polynomial` of the previous function `derivative`. Note: the function will be tested in the following manner: `derivative_tr p (-1, [])`.

    For example, `derivative_tr (4,[3.; 0.; 5.; 0.; 1.]) (-1, [])` returns `(3,[12.; 0.; 10.; 0.])`.