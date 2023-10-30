We want to represent positive integers using a binary representation.

This time we will use a type with boolean elements: `false` represents the bit `0` and `true` represents the bit `1`.
```ocaml
type liste_bool = Nothing | OneMore of bool * liste
```
In the following, for simplicity, we will refer to the booleans by their associated bits.

More formally, an integer is represented by a non-empty list of booleans where the head is the least significant bit.

For example, the list `OneMore(false, OneMore(true, OneMore(true, Nothing)))` represents the number `0*1 + 1*2 + 1*2² = 6`.

**Note**: In order for each number to have a unique binary representation, we disallow any _leading zeros_. In other words, the most significant bit is always `1` (`true`), except for the number `0`.

---

**Question 1**:
Write a function `is_binary: liste_bool -> bool` that determines whether a list represents a number written in base `2`.

**Question 2**:
Write a function `encode: int -> liste_bool` that calculates the binary representation of a positive integer (the behavior of `encode` for negative numbers is not specified).

**Question 3**:
Write the function `decode: liste_bool -> int` which, conversely, calculates the integer corresponding to a valid binary representation, taking into account the following note.

For this, we can observe that
```ocaml
2⁰ * a₀ + 2¹ * a₁ + 2² * a₂ + 2³ * a₃ + 2⁴ * a₄ + 2⁵ * a₅ + 2⁶ * a₆ + 2⁷ * a₇
```
can also be written as
```ocaml
a₀ + 2 * (a₁ + 2 * (a₂ + 2 * (a₃ + 2 * (a₄ + 2 * (a₅ + 2 * (a₆ + 2 * a₇))))))
```

**Question 4**:
Define the function `plus_bin: liste_bool -> liste_bool -> liste_bool` that calculates the addition in binary representation.

We can observe that, similar to decimal addition, binary addition can be done by starting from the least significant bits and potentially propagating a carry.

**Bonus**:
Implement multiplication, considering its efficiency.
