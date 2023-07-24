The game of Sudoku consists of filling a grid of size 9 x 9 in such a way that each row, each column, and each of the nine 3 x 3 squares contains exactly all the integers from 1 to 9.

In this exercise, we will verify if a grid is correctly filled. There are several ways to solve this, typically using for loops in imperative programming.

The objective here is to do it in a purely functional manner; there are several similar but distinct operations.

For clarity, we can declare _type aliases_ to describe all the elements of a Sudoku grid:

```ocaml
(* small line of three numbers *)
type small_line = int * int * int

(* small size block 3 x 3 *)
type block = small_line * small_line * small_line

(* large 3 x 9 line *)
type big_line = block * block * block

(* full grid *)
type grid = big_line * big_line * big_line
```

In the following sections, unless otherwise stated, **annotate each function using the types mentioned above**.

Attempting to solve this problem directly will be tedious: many parts of the code will be very similar but will apply to different types (small lines, blocks, etc.).

Since our grid consists of triplets of triplets of triplets of triplets, we will start by writing some utility functions that manipulate triplets. This will allow us to not manipulate triplets directly afterwards.

**Note:** All the following function definitions are simple. They fit in at most two lines: one for the type annotation in the declaration and one for the function body.

---

**Question 1: generic functions**

0. Define the projections `p1`, `p2`, and `p3` for triplets, similar to `fst` and `snd` for pairs, without annotating their types.

1. Write the function `app3` that takes a function as its first argument, a triplet as its second argument, and applies the function to each element of the triplet, returning the corresponding triplet. Avoid _annotating the type_ of this function and observe the type inferred by OCaml.

2. Define a function `all` that takes a function of type `bool` and a triplet, and checks if the function returns `true` for all elements of the triplet.

3. Given the same inputs, the function `exist` should return `true` if the function returns `true` for at least one element.

4. Finally, define a function `exist_pair` such that if `test` is a boolean function with two arguments and `t` is a triplet, then `exist_pair test t` returns `true` when there exists a pair `(a, b)` such that `test a b` is `true` for two distinct elements `a` and `b` from the triplet `t`. We can assume that the function `test` is symmetric: `test a b = test b a`.

The above functions are called high-order functions as they take a function as an argument. They will allow us to define functions with similar structures more easily.

---

**Question 2: boundary verification**

Let's first verify that all elements in the grid are integers between 1 and 9.

1. Write a function `within_bounds` that checks if a number is between 1 and 9.

2. Using `all` and `within_bounds`, define functions `within_bounds_small_line` and `within_bounds_block` that return a boolean indicating whether all elements in their argument are between 1 and 9. There is no need to explicitly specify the type argument for `block` to define this function!

3. Similarly, using `all` and `within_bounds_block`, define a function `within_bounds_grid: grid -> bool` that checks if all elements in a grid are between 1 and 9.

---

**Question 3: duplication verification**

We now want to check that there are no duplicates in a block.
1. Using the functions from question 1, define a function `differents_small_line: small_line -> bool` that checks if the elements of a small line are all pairwise different. In other words, there are no two equal elements.
2. Define a function `within` such that `within pl x` tests if `x` is in the small line `pl`.
3. Deduce a function `intersects` that takes two small lines and checks if they intersect. That is, if there is an element from the first line that is in the second line.
4. Deduce a function `differents_block: block -> bool` that checks if all the elements of a block are all different. This means verifying that in each small line, all the elements are different, and that there are no two intersecting small lines.

---

**Question 4. (Block Verification)**

Note that a block (3 x 3) contains exactly the numbers from 1 to 9 if all its elements are between 1 and 9 and if they are all different.

1. Deduce a function `block_correct: block -> bool` that checks if a block is well-formed.
2. Use it to define `blocks_correct: grid -> bool` that checks this for all the blocks in a grid.

---

We notice that the validity rule of a Sudoku grid is repetitive: it is the same condition for blocks, rows, and columns. Now that we know how to check a block, we will modify a grid so that its rows become blocks, and thus be able to reuse the previous function.

---

**Question 5. (Transposition)**

Use projections to define a function `transpose9` that transposes a 3 x 3 block: its rows become its columns and vice versa.

For example,
```
1 9 2          1 8 9
8 2 1  becomes 9 2 2
9 2 7          2 1 7
```

Check (using the "Compile" button) that it can be annotated with the following types:
```ocaml
: block -> blocf
: big_line -> big_line
: grid -> grid
```
Then remove its type annotations.

---

**Note:** To test your transformations on grids (like `transpose9`), feel free to use the `random_grid` and `print_grid` functions provided in the preamble.

---

**Question 6. (Row Verification)**

Use `transpose9` to define `transpose_lines_blocks: grid -> grid` that creates a grid in which each block is a row of the input (and vice versa).

For example,
```
1 9 2 | 8 2 1 | 9 2 7           1 9 2 | 7 9 8 | 2 1 3
7 9 8 | 5 4 9 | 4 3 5           8 2 1 | 5 4 9 | 9 7 4
2 1 3 | 9 7 4 | 4 7 9           9 2 7 | 4 3 5 | 4 7 9
------+-------+------           ------+-------+------
6 5 4 | 4 4 1 | 5 2 8           6 5 4 | 9 4 2 | 1 8 2
9 4 2 | 3 9 3 | 7 6 5  becomes  4 4 1 | 3 9 3 | 6 1 1
1 8 2 | 6 1 1 | 2 1 7           5 2 8 | 7 6 5 | 2 1 7
------+-------+------           ------+-------+------
2 9 5 | 2 1 7 | 9 3 3           2 9 5 | 4 1 8 | 9 3 9
4 1 8 | 3 7 6 | 6 7 3           2 1 7 | 3 7 6 | 3 1 3
9 3 9 | 3 1 3 | 5 8 1           9 3 3 | 6 7 3 | 5 8 1
```

Use this function to define `lines_correct: grid -> bool` that checks if all the rows of a grid are correct.

---

**Question 7. (Column Verification)**

Using `transpose9`, define a function `transpose_blocks: grid -> grid` that transposes each block of a grid (each block remains in place).

Deduce a function `transpose_grid: grid -> grid` that, similar to `transpose9`, creates a grid in which the rows are the columns of the argument.

For example,
```
1 9 2 | 8 2 1 | 9 2 7           1 7 2 | 6 9 1 | 2 4 9
7 9 8 | 5 4 9 | 4 3 5           9 9 1 | 5 4 8 | 9 1 3
2 1 3 | 9 7 4 | 4 7 9           2 8 3 | 4 2 2 | 5 8 9
------+-------+------           ------+-------+------
6 5 4 | 4 4 1 | 5 2 8           8 5 9 | 4 3 6 | 2 3 3
9 4 2 | 3 9 3 | 7 6 5  becomes  2 4 7 | 4 9 1 | 1 7 1
1 8 2 | 6 1 1 | 2 1 7           1 9 4 | 1 3 1 | 7 6 3
------+-------+------           ------+-------+------
2 9 5 | 2 1 7 | 9 3 3           9 4 4 | 5 7 2 | 9 6 5
4 1 8 | 3 7 6 | 6 7 3           2 3 7 | 2 6 1 | 3 7 8
9 3 9 | 3 1 3 | 5 8 1           7 5 9 | 8 5 7 | 3 3 1
```

Note that we only need to transpose the entire blocks and then within each block.

Finally, define `columns_correct: grid -> bool` following the pattern of `lines_correct`, and then `correct: grid -> bool` that checks all the
expected properties of a Sudoku grid.

**Bonus Questions**

  - Based on the previous functions, redefine the function `print_grid` (the version in the preamble is intentionally obscured).
  - (difficult) An incomplete grid can contain empty cells denoted by `0`. Define a function that produces a complete and correct grid from a grid with empty cells, if possible.
