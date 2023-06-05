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

Consider the following function `future`:

```ocaml
let rec future d r =
    if d = 0 then r 
    else 
      let a = (d lsr 1) in  
      let b = (string_of_int (d land 1)) in
        future  a  (b ^ r) 
```


1. Specify the type of the function `future`.

2. What value is returned when we run `future 13 ""`?

3. Assuming that, in the initial call, the `d` parameter is a natural integer and `r` is the empty string `""`, this function calculates:

    A) The function creates a palindrome from the parameter `d` <br />
    B) The function calculates the number of $1$'s present in the binary representation of `d` <br />
    C) The function generates the binary representation of `d` <br />
    D) The function generates a string of $1$'s and $0$'s such that the number of $1$'s is odd if parameter `d` is odd, and the number of $1$'s is even if parameter `d` is even <br />

**Notes:** 
 - Multiple choice: If you believe that the correct option is `A` then you should answer as follows: `let answer = A`.
 - Types: Write the type of the function in the definition of its corresponding type. Example: `type p1 = float -> int`.
