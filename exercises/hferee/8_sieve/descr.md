The Sieve of Eratosthenes is an algorithm that efficiently calculates all prime numbers smaller than a given number `n`.

The algorithm works as follows:
1. We build a list of numbers from 2 to `n`.
2. If `k` is the next number in the list, then it is prime, and we remove all multiples of `k` (except `k`) from the rest of the list.
3. We repeat step 2 as long as there are numbers remaining to be processed.

**Question 1**:
Write a function `range: int -> liste` that, given an integer `n`, constructs the ordered list of numbers from `2` to `n`.

**Question 2**:
Define the function `filter: (int -> bool) -> liste -> liste` that filters the elements of a list based on a function with a boolean output.

**Question 3**:
Implement the sieve algorithm in a recursive function `sieve`.

<details>
    <summary>Hint</summary>
    Use the `filter` function.
</details>

**Bonus**:
Calculate `sieve 100000` in less than 20 seconds.
