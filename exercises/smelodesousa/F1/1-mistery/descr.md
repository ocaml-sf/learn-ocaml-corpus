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

Consider the following function `mistery`:

```ocaml
let rec mistery x y z = 
  if y <= 0 then z else mistery x ( y - 1) (x * z)
```

1. Provide its type.

2. What is the value returned by `mistery 2 3 1`?

3. Assuming that the parameters are all natural integers and that the initial value of `z` is $1$, this function determines the values of:

    A) The function: $(x,y) \mapsto y^x$<br />
    B) The function: $(x,y) \mapsto y^x-1$<br />
    C) The function: $(x,y) \mapsto x^y$<br />
    D) The function: $(x,y) \mapsto (x-1)^y$<br />

**Notes:** 
 - Multiple choice: If you think that the correct option is `A` then you should answer as follows: `let answer = A`.
 - Typing: Write the function's type in the corresponding definition. Example: `type p1 = float -> int`.