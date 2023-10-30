We will represent the famous fractal known as the "Sierpiński triangle" using ASCII characters.

The fractal is defined by its level of detail.
- Level `0` consists of a single character "*".
- The next level is the simple triangle:

```
 *
* *
```

- Each level is constructed from the previous level by making three copies of the previous level and arranging them in a triangular shape. For example, level 4 looks like this:

```
               *
              * *
             *   *
            * * * *
           *       *
          * *     * *
         *   *   *   *
        * * * * * * * *
       *               *
      * *             * *
     *   *           *   *
    * * * *         * * * *
   *       *       *       *
  * *     * *     * *     * *
 *   *   *   *   *   *   *   *
* * * * * * * * * * * * * * * *
```

---

**Question 1**

Let's start by determining the number of lines and columns required to represent the triangle at level `n`.

Define a recursive function `size_sierpinsky: int -> int * int` that calculates the height and width of the triangle at a given level.

---

**Question 2**

Before drawing the fractal, let's represent it with a function.

Define a function `s: int -> int -> int -> string` such that `s n i j` returns the string (either `" "` or `"*"`) at line `i` and column `j` of the Sierpiński triangle at level `n`. Assume that `i` and `j` are between `0` and the dimensions given by `size_sierpinsky`.

---

**Question 3**

We now want to draw the Sierpiński triangle.

Define a function `draw: (int -> int -> string) -> (int * int) -> unit` such that `draw f (l, c)` displays `l` lines and `c` columns, with the characters at line `i` and column `j` given by `f i j`.

You can use the `print_string` and `print_newline` functions for this.

For example, `draw (fun x y -> if x = y then "A" else "B") (4, 6)` will display:
```
ABBBBB
BABBBB
BBABBB
BBBABB
```

---

**Question 4**:

Use the previous functions to define the function `sierpinsky: int -> unit` that displays the Sierpiński triangle at a given level. Check what it produces up to `n = 5`.
