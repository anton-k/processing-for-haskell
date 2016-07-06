Vector space
================================

In processing almost all drawing functions take plain numbers as arguments.
But in Haskell lib they take in vectors or pairs of numbers. Why do we need this?

The representation of points is much more convenient with vectors. 
The vectors come with the library vector-space and it provides many 
useful functions.

We can treat the vectors like numbers. We can add, multiply, negate them, create them
out of numbers. The number `12` can become a vector `(12, 12)` with the help of Haskell overloading.
We can scale vectors with numbers. So instead of writing:

~~~haskell
width = 400
height = 400

center = (0.5 * width, 0.5 * height)
~~~

We can rewrite it:

~~~haskell
sizes = (400, 400)
center = 0.5 *^ sizes
~~~

There are another usefull functions:

* `distance` calculates the distance between two vectors.

* `magnitude` calculates the size of the vector

* `lerp` interpolates between two vectors

* `normalized` calculates a normalized vector for the given one.

You can read the whole list of functions in the package [`vector-space`](https://hackage.haskell.org/package/vector-space-0.10.2) on Hackage.
