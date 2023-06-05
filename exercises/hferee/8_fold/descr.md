Throughout this exercise, we will consider a type `liste` for a list of integers and `fold_right: (int -> 'a -> 'a) -> liste -> 'a -> 'a` as its right folding function.

**Note**: In the following exercise, you won't need to know the precise implementation of lists. You should use the `fold_right` function with the appropriate arguments.

---

**Question 1**:

Using the `min` and `max` functions for integers, define the functions `list_min` and `list_max` that respectively calculate the minimum and maximum of a list. In the case of an empty list, you should return `min_int` or `max_int`.

---

**Question 2**:

Define the functions `count_if`, `forall`, and `exists` such that if `p` is of type `int -> bool` and `l` is a list, then:
  - `count_if p l` returns the number of elements in `l` for which `p` is `true`.
  - `forall p l` returns `true` if and only if `p` is true for all elements of `l`.
  - `exists p l` is true if `p` is true for at least one element of `l`.

---

**Question 3**:

Using `exists`, define the function `mem: int -> liste -> bool` (for *member*) that determines whether an element is contained in a list.

---

**Bonus**:

Define the functions `find_first` and `find_last` that return the first and last occurrences, respectively, satisfying a test `p`.
