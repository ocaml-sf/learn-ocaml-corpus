# Introduction

A zombie invasion has affected your neighborhood. Your neighborhood was doomed if not for the cats, which are inherently resistant to disease and zombie predators.

The neighborhood is shaped like a board of *n* by *n*, with a person, cat, or zombie alternately occupying each cell.

A zombie attacks its neighbors from above, below, right, and left. If a zombie attacks a man, it transforms into one afterward and begins to attack its neighbors (the ones above, below, left, and right). In addition, when a cat is attacked, the attack is simply canceled, and the cat continues to be a cat.
Lastly, when a zombie is attacked by another zombie, they look at each other with what is left of their eyes and cancel the attack with an apologetic grunt.

Your task is: given a board filled with an initial configuration, find the final configuration. Will there be any survivors?

An example of an initial configuration might be:

![zombie-0](https://i.imgur.com/RXsUuzU.png) 

In this case, the final configuration is:

![zombie-11](https://i.imgur.com/dbRhGAv.png)


# Objectives

Define the function `zombie_attack : char array array -> char array array`, which receives the initial configuration in the form of a square array of characters, calculates and returns the final configuration.

The characters that make up the matrix are alternatively the character * ("asterisk", representing a cell that harbors a zombie), the character X (representing a cell that contains a brave cat), or the character . ("dot", representing a cell that contains an innocent passer-by).

The matrix will not be larger than 1000.

Input examples:

```
*...*..
..XX...
.X..X..
..X..X.
X.X.X..
.X.X...
X.....*
```

Output examples:

```
*******
**XX***
*X..X**
**X..X*
X*X.X**
.X*X***
X******
```