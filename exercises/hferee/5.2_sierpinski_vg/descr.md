Now we are going to draw the fractal known as the "Sierpiński triangle" to create a vector image.

In the following, all the images will have a size of `1.0 x 1.0`, with the origin (i.e., the point with coordinates `(0., 0.)`) at the bottom-left.

To do this, we will define paths, represented by the type `path`. The act of drawing will then be a function that modifies a path, so it will have the type `path -> path`.

To draw, we will only need the following primitives:
- `triangle: float -> path -> path`, where `triangle size p` extends the path `p` by adding an isosceles triangle of side length `size` to the end of it.
- `move_by: float -> float -> path -> path`, where `move_by x y p` extends the drawing `p` without drawing anything, by moving horizontally a distance of `x` and vertically a distance of `y` from the end of the path `p`.

Once a drawing `d: path -> path` is defined, we can transform it into an image using the function `draw: (path -> path) -> image`. The global declarations of type `image` will then be displayed in the OCaml toplevel. Remember to use the "Eval code" button to visualize your drawing.

---

**Question 1**:
Define the image `one_triangle: image` that contains a triangle of size `1.`.

**Question 2**:
Define the image `two_triangles: image` that contains two triangles, each half the size of the other, placed next to each other.

Note: The `triangle` function starts and ends the drawing at the bottom-left.

---

The Sierpiński triangle of depth `0` is the triangle of size `1.`, and the Sierpiński triangle of depth `n + 1` is composed of three Sierpiński triangles of size `n`, each half the size, arranged in a triangle shape.

![The Sierpiński triangle of depth 6](/exercises/5.2_sierpinski_vg/images/sierpinski6.png "The Sierpiński triangle of depth 6")

---

**Question 3**:
Define the function `sierpinski: int -> image` that draws the Sierpiński triangle at a given depth.

We can start by writing a recursive function `draw_sierpinski: float -> int -> path -> path` such that `draw_sierpinski s n` draws the Sierpiński triangle of size `s` and depth `n`.

**Remarks**:
- We can observe that the height of a Sierpiński triangle is equal to <script type="math/asciimath">sqrt(3/4)</script> times its width.
- The drawing of the simple triangle starts and ends at the bottom-left corner. The same applies to the triangle at any level.
- To pass the test, the drawing must be done in a specific order: First, the triangle at the bottom-left, then the one at the bottom-right, and finally the one at the top.

**Note**: We can test `draw_sierpinski 8`, but we may not notice any significant difference with larger depths.
