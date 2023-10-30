# Introduction

Consider the problem of optimizing taxi routes in modern cities like Manhattan, for example. Consider the following map where the roads are grayed out. Also consider that the buildings in the city, which are represented by the white squares, have dimension one.

![](https://i.imgur.com/A4UYbex.png)

What is the shortest taxi distance between the lower left point and the upper right point?

If we could fly, the Euclidean distance would be the solution (the green route). For the distance traveled by car, however, the calculation must be different. Therefore, what is the length of the red route? Or the blue route? What about the yellow route? 

# Goals

Based on the answer to these questions, propose the implementation in OCaml of the function `manhattan_distance : int -> int -> int -> int -> int` such that `manhattan_distance x y a b` calculates the Manhattan distance between the point **(x,y)** and the point **(a,b)**.
