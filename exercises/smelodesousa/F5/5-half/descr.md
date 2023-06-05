# Objective

Define the function `halve : int list -> (int list * int list)` that takes in a list as an argument and cuts it in half. In the case of an odd-length list, the middle element stays on the rightmost list.

As an example, `halve [1;2;3;4;5;6;7;8;9] = ([1;2;3;4],[5;6;7;8;9])`.

It is important to note that there is a solution that uses an auxiliary tail recursive function (with the same parameter configuration as `halve`), that only needs to run through the complete list and its first half once.