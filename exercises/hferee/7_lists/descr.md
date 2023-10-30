Condider the following type of integer lists :

```ocaml
type liste = Nothing | OneMore of int * liste
```

The goal here is to master basic functions on lists.

---

**Question 1**:

Define the function `hd` that calculates the head of a list. We will return an arbitrary value in case the list is empty.

Similarly, define the function `tl` that calculates the tail of a list.

---

**Question 2**:

Define the functions `length` and `sum_list` that respectively calculate the length and the sum of the elements in a list.

---

**Question 3**:

Define the `concat` function that concatenates two lists, as well as the `rev` function that reverses the order of a list.

---

**Question 4**:

Define the `mem` function that takes an integer and a list and returns a boolean indicating whether the integer belongs to the list.

Similarly, define the `find_first` and `find_last` functions that respectively return the first and last position of an integer in a list. We will return `-1` if the integer is not found.

---

**Question 5**:

Define a function `partition: (int -> bool) -> liste -> liste * liste` such that `partition p l = (l1, l2)` where `l1` (respectively `l2`) contains the elements from `l` that satisfy the predicate `p` (respectively do not satisfy `p`), in the same order.w
