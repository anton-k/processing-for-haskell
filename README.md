## Processing for Haskell

Computer Graphics for kids and artists! 
It's an imperative EDSL for computer graphics. It's very easy to use. 
The library implements Processing language in Haskell. 

Well, But actually it...

... implements a **subset** of Processing Language in Haskell. So ...


### How to install

You can install it from hackage.

~~~
> cabal install processing-for-haskell
~~~

### Guide

If you are familiar with processing you can read:

* [Quick start guide for Processigers](https://github.com/anton-k/processing-for-haskell/blob/master/tutorial/QuickStartForProcessingers.md)

Also you can read more detailed tutorial. The code examples for tutorial can be found [here](https://github.com/anton-k/processing-for-haskell/tree/master/tutorial/code):

* [First steps](https://github.com/anton-k/processing-for-haskell/blob/master/tutorial/FirstSteps.md)

* [Simple shapes](https://github.com/anton-k/processing-for-haskell/blob/master/tutorial/Shapes.md)

* [Transformations](https://github.com/anton-k/processing-for-haskell/blob/master/tutorial/Transformations.md)

* [User input](https://github.com/anton-k/processing-for-haskell/blob/master/tutorial/UserInput.md)

* [Randomness](https://github.com/anton-k/processing-for-haskell/blob/master/tutorial/Random.md)

* [Vectors](https://github.com/anton-k/processing-for-haskell/blob/master/tutorial/VectorSpace.md)

There are many examples to try out at the [examples](https://github.com/anton-k/processing-for-haskell/tree/master/examples) directory.

### Missing features

* Image processing functions

* 3D drawing

* Textures

* Text and font rendering

* Functions for rendering of complex 2D shapes (polygons with holes)

* Should check for perlin-noise implementation.

### The project needs your help

The Processing being a small language implements some  tons of
magic with OpenGL  under the hood. There are really great implementations
of graphics primitives. I can not finish this thing all alone.
So if you are really interested in seeing the package finished.
If you want all features of Processing be implemented please **do** contribute!

