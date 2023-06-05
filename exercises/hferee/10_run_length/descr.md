# Encode by Range

Range encoding is a lossless data compression algorithm. It involves indicating the number of times an element should be represented.

For example, the sequence of letters `aabbbabb` would be represented as `a2b3a1b2`.

**Question 1**:
Write a function `encode` that encodes a list (of arbitrary elements) into a list of pairs representing its range encoding.

For example,
```ocaml
encode [1; 1; 1; 2; 2; 1; 2; 2] = [(1, 3); (2, 2); (1, 1); (2, 2)]
```

**Question 2**:
Define the function `decode` which satisfies the following properties for any lists `l` and `lp`:
```ocaml
encode (decode lp) = lp
decode(encode l) = l
```

**Question 3**:
Observe the following sequence:
```
1
1 1
2 1
1 2 1 1
1 1 1 2 2 1
3 1 2 2 1 1
1 3 1 1 2 2 2 1
1 1 1 3 2 1 3 2 1 1
…
```

Define the function `mystery` such that for any positive integer `n`, `mystery(n)` calculates the n-th line of this logical sequence, with `mystery 0 = [1]`.

**Bonus**:
Define the function `list_mystery` which, given a positive integer `n`, represents the first `n + 1` lines of the logical sequence.
