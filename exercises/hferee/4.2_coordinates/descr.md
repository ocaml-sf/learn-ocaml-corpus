# It's time to coordinate, point!

The following record types represent two ways of representing points in the plane.

```ocaml
type cartesian = { x: float; y: float; }
type polar = { r: float; angle: float; }
```

**Question 1:**

Write a function `cartesian_of_polar` that converts a point in polar coordinates to cartesian coordinates.

**Question 2:**

Define the function `milieu_cart: cartesian -> cartesian -> cartesian` that calculates the midpoint of two points.

We now define the type `point` that can represent points in the plane using either of the representations.

```ocaml
type point = Cartesian of cartesian | Polar of polar
```

**Question 3:**

Define a function `milieu: point -> point -> point` that calculates the midpoint of two points, regardless of their representation.

