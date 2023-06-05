**Question 1**:

It is often useful to be able to display an OCaml value on the screen as a string, for example when debugging a program. A _printer_ for type _t_ is a function of type `t -> string`.

```ocaml
type 'a show = Show of ('a -> string)
```

The goal of this exercise is to write a small library for manipulating generic printers, defined as the type `'a show` above.

1. Write a function `print: 'a show -> 'a -> unit` such that `print s v` prints the value `v` to the standard output, followed by a newline.

2. Define the printers `show_int: int show`, `show_float: float show`, and `show_string: show string` for integers, floats, and strings. Strings should be displayed between double quotes, for example `print show_string "toto"` should print `"toto"` to the standard output, including the quotes.

3. Write a printer `show_string_l: string show` that displays a string along with its size in curly braces. For example, `print show_string_l "toto"` should print `"toto"{4}` to the standard output.

4. Write a function `show_pair: 'a show -> 'b show -> ('a * 'b) show` that constructs a printer for a pair from printers for each of its components. For example, `print (show_pair show_int show_float) (1, 42.5)` should print `(1, 42.5)` to the standard output.

5. Write a function `show_list: 'a show -> 'a list show` that constructs a printer for a list from a printer for its elements. The contents of the list should be displayed between square brackets and separated by semicolons. For example, `print (show_list (show_pair show_float show_string_l)) [(1.5, "foo"); (12., "bar")]` should print `[(1.5, "foo"{3}); (12., "bar"{3})]` to the standard output.

6. Use `show_list` to define a variant `show_list_lines: 'a show -> 'a list show'` that displays each element on a separate line with an indentation at the beginning of the line. In particular, `print (show_list_lines show_int) [1; 2; 3]` will display
```
[
 1
 2
 3
]
```

   Deduce a printer `show_list_list: 'a show -> 'a list list show` that, given a printer for `'a`, displays a list of lists line by line, where each line represents elements of type `'a`.
