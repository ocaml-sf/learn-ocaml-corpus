# Merge Sort

In this short exercise, we implement **merge sort** on lists.

Then, we implement a function that removes **consecutive duplicate elements**
in a list.

We combine them to obtain a function that removes all **duplicate elements**
in a list, even if they are not adjacent, and produces a sorted list.

**Question 1.** Implement two functions `even` and `odd`, both of type `'a
list -> 'a list`, such that `even` returns the sublist of the elements found
at even positions in the list `xs`, and `odd` returns the sublist of the
elements found at odd positions in the list `xs`. (By convention, the front
element of a list is at index 0.)

*Hint.* The functions `even` and `odd` should be mutually recursive.

In the following, we restrict our attention to lists of **integer elements**,
and we equip the type of integers with a fixed ordering,
namely the natural ordering `(<=)`.
We say that a list is **sorted** if it is sorted with respect to this
ordering, that is, if its elements appear in increasing order.

In a real-world implementation,
one would make the code polymorphic by parameterizing it over an equality `(=)`
and over an ordering `(<=)` that is compatible with this equality.

**Question 2.** Implement a function `merge` of type
`int list -> int list -> int list`
such that,
assuming that the lists `xs` and `ys` are sorted,
the list `merge xs ys` is also sorted,
and the multiset of its elements
is the union of the multisets of elements of the lists `xs` and `ys`.

When implemented in the most natural way,
the functions `odd`, `even`, and `merge`
have linear time complexity.
Indeed, when one of these functions is applied to a list,
a constant amount of work is performed,
followed with a recursive call on a strictly smaller list.

**Question 3.** Implement a function `sort` of type
`int list -> int list`
such that
the list `sort xs` is sorted
and the multiset of its elements
is the multiset of the elements of the list `xs`.

*Hint.* If the list `xs` can be divided in two strictly smaller sublists, then
sort these sublists, and merge the resulting sorted sublists. Otherwise, there
is nothing to do.

The worst-case time complexity of merge sort is O(nlog n).
Indeed, when it is applied to a list of size n, a linear
amount of work is performed (within `even`, `odd`, and `merge`),
and two recursive calls take place on lists that are twice smaller.
Thus, roughly speaking, the cost of merge sort satisfies the
recursive equation cost(n) = O(n) + 2*cost(n/2).
One can prove that the solutions of this equation grow like nlog n,
up to a constant factor.

**Question 4.** Implement a function `uniq` of type
`int list -> int list`
such that
the list `uniq xs`
is the list `xs`
where every run of several consecutive equal elements
is replaced with a single copy of this element.

The functions `sort` and `uniq` can be combined
to obtain a function that removes all duplicate elements
in a list, even if they are not adjacent.

**Question 5.** Implement a function `weed` of type
`int list -> int list`
such that
the list `weed xs` does not contain two copies of a single element
and the set of its elements is the set of elements of `xs`.
