# Breaking a Sort

This exercise is about detecting bugs through black-box testing.

More specifically, your task is to **detect bugs in sorting algorithms**.

For simplicity, we restrict our attention to sorting **integer lists**
according to the **standard ordering** of integers.

You are given a list of candidate sorting functions:

```
  val sorts: (int list -> int list) list
```

The catch is, **all of these sorting functions are flawed** in some way.

It is up to you, in each case, to find a flaw and exhibit an
input list that triggers the flaw. We refer to such an input
as *a problematic input*.

Thus, your task is to propose a list of problematic inputs:

```
  let inputs : int list option list =
    ... (* up to you *)
```

The length of the list `inputs` should be the length of the list `sorts`. For
each sorting function `sort`, you are expected to propose an input `Some xs`
such that the function call `sort xs` does not produce the expected result.
If you cannot find an input that triggers a flaw, you can propose `None`.

This exercise is graded as follows:

* Proposing a problematic input of optimal length yields 5 points.

* Proposing a problematic input of suboptimal length yields 2 points.

* Not proposing anything yields 0 points.

* Proposing an input which turns out not to be problematic yields -5 points.

It is permitted to use the standard library `List.sort`
as a reference implementation.

*Hint.* Use a combination of exhaustive testing and random testing. For
instance, it is useful to test all input lengths up to a certain bound.
On the other hand, as `n` becomes large, it is infeasible to enumerate
all lists of length `n`, even if list elements are picked from a small
interval. Therefore, it becomes necessary to pick elements at random.

*Hint.* Random testing usually leads to discovering problematic inputs
that are larger than necessary. Once you have discovered a problematic
input, try to *shrink* it, that is, to make it shorter, while preserving
the property of being problematic.
