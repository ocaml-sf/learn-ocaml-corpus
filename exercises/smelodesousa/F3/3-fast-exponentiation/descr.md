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

This exercise is centered around the integer exponentiation function $x^n$. The natural and direct definition of this operation is $x^n = \underbrace{x \times x \times \ldots \times x}_{n\ ti mes}$ implying $n$ multiplications. However, there is a simple alternative definition that allows us to perform the same operation more efficiently:

<center>$
x^n = {
(1, if n=0),
(x, if n=1),
(x^{\frac{n}{2}} \times x^{\frac{n}{2}}, if n\ is\ even),
(x^{\frac{n-1}{2}}\times x^{\frac{n-1}{2}}\times x, if n\ is\ odd
):}
$</center>


This method of exponentiation is sometimes called **fast exponentiation**.


# Objectives

1. Propose a recursive function in OCaml that implements this definition.

2. What is the complexity of this algorithm (in terms of the number of multiplications made)?

    A) $\mathcal{O}(n)$ (linear) <br />
    B) $\mathcal{O}(n^2)$ (quadratic)<br />
    C) $\mathcal{O}(n * log_2(n))$ <br />
    D) $\mathcal{O}(log_2(n))$(logarithmic)<br />
    E) $\mathcal{O}(1)$ (constant)<br />
    F) $\mathcal{O}(n * log^2(n))$<br /><br />

**Note:** If you believe that the correct option is *`A`* then you should answer as follows: *`let answer = A`.
