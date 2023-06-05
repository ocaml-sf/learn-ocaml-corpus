# Introduction

The **Burrows-Wheeler transform (BWT)** is a pretreatment process for data compression, invented by Michael Burrows and David Wheeler in 1984 (after the first results by D. Wheeler). It is not a compression algorithm as it does not reduce the size of the processed text, but _BWT_ has the property of calculating permutations of the text that group together similar characters. These groupings make the resulting text a particularly interesting candidate for _RLE_ type methods (look at the exercise in this sheet).

It is a technique used in compression systems such as _bzip2_ or in the area of computational genomics, where it finds applications in finding sequence alignment or repeatitions.

More details at: 

- Michael Burrows, D. J. Wheeler: ["A block-sorting lossless data compression algorithm"](http://citeseer.ist.psu.edu/76182.html), 10th May 1994, Digital SRC Research Report 124.

- [Article by Dr. Dobb's on Burrows-Wheeler](http://marknelson.us/1996/09/01/bwt/)

# Objectives

The purpose of this exercise is to implement the encoding and decoding processes. To do so, we will illustrate the process with a complete example for each one.

We intend to encode with BWT the word "ANAGRAMA".

Encoding:

First, we create a word-sized square character matrix of the uncoded word. This matrix is filled by doing a right rotating _shift_.


```pseudocode
   matrix     

A N A G R A M A
A A N A G R A M
M A A N A G R A
A M A A N A G R 
R A M A A N A G
G R A M A A N A
A G R A M A A N
N A G R A M A A
```

Then, you sort the rows of this matrix alphabetically.

```pseudocode
   matrix      # line

A A N A G R A M   1
A G R A M A A N   2
A M A A N A G R   3 
A N A G R A M A   4
G R A M A A N A   5
M A A N A G R A   6
N A G R A M A A   7
R A M A A N A G   8
```



The encoding will be the pair `(4, "MNRAAAAG")`. The 4 is the line number where the original word is in the sorted array. The word "MNRAAAG" is the word made up of the letters in the last column, from top to bottom.

The decoding starts at `(4,"MNRAAAAG")` and finds the word "ANAGRAMA" without the knowledge of this matrix.

- Write a function `bwt: string -> int*string` that encodes a word with the BWT method. 

  Therefore, `bwt "anagrama" = (4,"mnraaaag")`.

- Write a function `debwt : int * string -> string`.  For example, `debwt (4,"mnraaag") = "anagrama"`.