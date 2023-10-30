We want to sort a list of integers in ascending order.

**Question 1**:
Define a recursive function `insert: int -> liste -> liste` that inserts an element into a list in such a way that the output list is sorted, assuming the input list was already sorted.

**Question 2**:
Using `fold_right` and `insert`, define a function `sort` that sorts a list by successively inserting all its elements.

**Bonus**:
Using `fold_right`, define a function `test_sort: liste -> bool` that indicates whether a list is sorted or not.
