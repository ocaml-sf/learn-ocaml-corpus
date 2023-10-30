Recall that a positive integer is considered _prime_ if it has exactly two positive divisors. Thus, `0` and `1` are *not* prime.

We are trying to determine whether a number is prime.

---

**Question 1:**

Define a function `div : int -> int -> bool` such that `div k n` indicates whether k divides n.

---

**Question 2:**

Define a recursive function `dividers : int -> int -> int` such that `dividers n k` counts the number of divisors between `1` and `k` (inclusive).

---

**Question 3:**

Use the previous function to define `prime : int -> bool` that decides whether a number is prime.

---

**Bonus:**

Rewrite `prime` in a more efficient manner. For instance, verify that `prime max_int` can be computed in less than 10 seconds.
