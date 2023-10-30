A strand of DNA is composed of a sequence of bases, which come in four types: `A`, `C`, `G`, and `T`.

One way to represent a DNA strand in OCaml is to use a string that contains only these four letters. This is certainly not the best representation of DNA strands in OCaml, but the goal here is to practice manipulating strings.

---

**Note:**

The string functions we will need here are as follows:
- The concatenation operator: `^` (infix)
- The function `String.length: string -> int` that calculates the length of a string.
- The function `String.get: string -> int -> char` that allows us to obtain the n-th character (e.g., `'A'`) of a string. Be careful to only call it with an integer between `0` and the length of the string (exclusive) to avoid getting an exception.

---

**Question 1:**

Write a function `is_dna: string -> bool` that determines whether a string represents a DNA strand. We can start by defining a recursive auxiliary function that takes a parameter representing a position `p` in the string and indicates whether the characters up to position `p` are correct.

---

From now on, we assume that the strings passed as arguments to functions are valid DNA strands.

Each DNA double helix is composed of two strands, with their bases paired: each `A` base is facing a `T` base, and each `G` base is facing a `C` base.

**Question 2:**

Write a function `complement` that, given a DNA strand, constructs the complementary strand.

---

**Question 3:**

In a simplified manner, a DNA strand encodes a list of proteins delimited by "stop codons," which are one of the three sequences: `"TAA"`, `"TAG"`, and `"TGA"`. Write a function `first_stop: string -> int` that returns the position of the first stop codon. For example, `first_stop "ACGTAGCT"` returns `3`. In the case where there is no stop codon, we will return the length of the string instead.
