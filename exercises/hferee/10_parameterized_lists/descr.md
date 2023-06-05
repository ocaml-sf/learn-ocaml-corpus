In the standard library of OCaml, lists are defined as follows:

```ocaml
type 'a t = 'a list =
  | []
  | (::) of 'a * 'a list
```

This means that for every type `t`, we can define and manipulate lists whose elements have the type `t`.

**Question 1**

Define lists `l3_int`, `l3_float`, `l3_bool`, `l3_int_int`, each containing three elements and respectively containing integers, floats, booleans, and pairs of integers.

**Question 2** _(no automatic validation)_

Observe the type of the list `empty`:
```ocaml
let empty = []
```

What types of elements can be added to this list? Test it in the toplevel.

Define a variable that represents an empty list to which only booleans can be added. Check its type and test in the toplevel that nothing else can be added to it.

------

This parameterized definition of the list type allows us to define polymorphic functions that work on all lists, regardless of the type of their elements.

**Question 3**

Define the polymorphic functions `length`, `map`, `fold_left`, and `fold_right`. Observe their types.
