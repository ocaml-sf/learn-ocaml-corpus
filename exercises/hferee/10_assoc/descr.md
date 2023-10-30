We recall the parameterized type `option`:
```ocaml
type 'a t = 'a option =
| None
| Some of 'a
```

If `t` is a type, values of type `t option` represent at most one value of type `t`.

A typical example of using the `option` type is the definition of partial functions. For example, a function of type `int -> string option` sometimes returns a string, and sometimes nothing.

------

**Question 1**:
Define a function `option_map: ('a -> 'b) -> 'a option -> 'b option` that applies a function to an argument that may or may not be a value. We can try to use this function in the subsequent questions.

**Question 2**:
Define the functions `hd_opt: 'a list -> 'a option` and `tl_opt: 'a list -> ('a list) option` that respectively calculate the head and tail of a list, if possible.

**Question 3**:
Using the `max` function, define the function `list_max`, which returns an option value representing the largest element of the list, if it exists.

For example, `list_max [1; 3; 2] = Some 3` and `list_max [] = None`.

Observe the type of `list_max`.

Generalize it to a function `list_greatest` such that `list_greatest le l` returns the greatest element of `l` according to the order `le: 'a -> 'a -> bool` (`le a b = true` if `a` is smaller than `b`).

**Question 4**:
Define the function `find_opt: ('a -> bool) -> 'a list -> 'a option` such that `find_opt f l` returns the first element of `l` that satisfies `f`, if it exists.

**Question 5**:
Define `filter_map: ('a -> 'b option) -> 'a list -> 'b list` such that `filter_map f l` returns the list (in order) of values `v` such that `f x = Some v` and `x` is an element of `l`.

-----------

An association list is a list of the form `('a * 'b) list`. They are particularly useful for storing data in the form of a pair `(key, value)`.

**Question 6**:
Write a function `assoc_opt` that takes a key, an association list, and returns, if it exists, a value associated with that key in the list.

**Question 7**:
We represent the student notes in a list associating a student's name (of type `string`) with their grade (of type `float option`); the grade `None` represents an absence:
```ocaml
type grades = (string * float option) list
```

The `assoc_opt` function allows us to retrieve the grade associated with a student.

Using only standard list functions, and without defining any recursive functions, define the following functions that take a list of type `grades` as an argument:

- The `average` function calculates the average grade considering absences as `0`.
- The `average_present` function calculates the average grade ignoring absences.
- The `max_grade` function returns the highest grade.
- The `best` function returns the name of a student with the highest grade.
