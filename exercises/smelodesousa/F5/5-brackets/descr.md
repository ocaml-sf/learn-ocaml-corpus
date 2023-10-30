# Introduction

Having a text with well-placed parentheses, in other words, with the symbols "(" and ")" properly used, is a common necessity, and of great importance when these texts are subject to computer processing (think of a compiler, for example).

In this exercise, we challenge you to check the proper use of parentheses in any text organized in the form of a list of characters.

A text has well-placed parentheses if any occurrence of '(':

- is associated with an occurrence of ')' *at the right*;
- also has well-placed parentheses in the text between itself and the corresponding ')'.

Note that, as a result, concatenating two texts with well-placed parentheses or putting a text with well-placed parentheses between parentheses also results in a text with correctly placed parentheses.

# Goal

Define a function `verify : char list -> char list -> bool` which checks if the text contained in the first parameter has well-placed parentheses. It is suggested that you use the second parameter (another `char list`, which we will define the accumulator) to _accumulate_ the intermediate scan results. Therefore, the accumulator is empty at the beginning of the analysis, and if during the check the accumulator has a parenthesis '(' as its first element, then the current status of the analysis still waits for a parenthesis ')'  which corresponds to the closing of the parenthesis in the accumulator. A proper use of this accumulator makes checking much easier!

For example:

`verify ['a';'(';'a';'b';'(';'b';')';'c';'(';'o';'k';'a';')';'n';')';'h'] [] = true`