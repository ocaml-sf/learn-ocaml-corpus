# Introduction

Let's implement a classic and simple lossless data compression method known as a _run-length encoder_ (RLE).

This method allows sequences of elements to be compressed and stored as a single data value and count.

It is efficient when the considered sequences are known to have multiple repeated occurrences. It is used alongside other compression methods that create such repetitions (such as the _Burrows-Wheeler_ method) to compress images, FAX messages, and more.

As an example, if we have a list with repeated characters: 

`aaiojanbeeebbaffssjjjjdreghsrf` is compressed to `a2iojanbe3b2af2s2j4dreghsrf`.

The general rule applied to any sequence of characters is: character `x` of length `y` is substituted by `xy`, which means "`x`, `y` times". The RLE codification is a simple application of this basic rule. Decodification, which allows for the recreation of the original string, is simply this process reversed.

# Goals

1.  Given an element `x` of an uncompressed list, we mean to define its image as per the RLE codification. If there is only one occurrence, then the codification should return `One x`, if it is the first element of a repetition of length `y`, it should return `Many (x, y)`. Which of these options correctly defines the type `rle_contents`:

    (a) `type rle_contents = int * (int*int)`<br />
    (b) `type rle_contents = One of int | Many of (int*int)`<br />
    (c) `type rle_contents = One | Many`<br />
    (d) `type rle_contents = { One : int; Many : int*int }`<br /><br />

2. Define the function `rle_encode : int list -> rle_contents list` that calculates the codification of the list passed as a parameter. For example `rle_encode [1;1;3;3,3;2;5;5]` returns `[Many (1,2); Many (3,3); One 2; Many (5,2)]`.

3. Define the inverse function `rle_decode :  'a rle_contents list -> int list`.
For example `rle_decode [Many (1,2); Many (3,3); One 2; Many (5,2)]` returns `[1;1;3;3,3;2;5;5]`.