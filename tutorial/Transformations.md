Transformations
========================================

In computer graphics we often draw not in absolute coordinates of the screen
but in some local coordinates of the object that we want to place on the scene.
This can be very convenient. We can ceate a separate procedure that
draws an object in it's own coordinate system and then we can 
place it anywhere on the screen.

Let's create a hero:

~~~Haskell
drawHero = do
	drawHead
	drawBody
	drawLegs
	drawArms

drawHead = do	
	fill (grey 165)	
	circle 10 (0, -20)

drawBody = do
	fill (grey 68)
	rect (-10, -10) (20, 40)

drawLegs = do
	fill (grey 125)	
	rect (-7, 30) (3, 27)
	rect (4, 30) (3, 27)
	
drawArms = do
	fill (grey 125)	
	strokeWeight 2
	linePath [(-10, -10), (-20, 7), (-15, 15)]
	linePath [(10, -10), (20, -27), (15, -35)]
~~~

### Translate

If we draw our hero on the screen he is going to stick to the upper left  corner.
But we can place him in the center if we translate the coordinates:

~~~Haskell
width = 400
height = 400

center = (0.5 * width, 0.5 * height)

draw () = do
	background (grey 230)
	translate center
	drawHero
~~~

So you can see that the command `translate` affects all shapes that are drawn after it.
After it the point `(0, 0)` coressponds to the `center`.

### Scaling

The next thing we can do is to change the size of our hero. We can make him bigger:

~~~Haskell
draw () = do
	background (grey 230)
	translate center
	scale (2, 2)
	drawHero
~~~

### Rotation

We can make him fly:

~~~Haskell
draw () = do
	background (grey 230)
 	translate center
 	scale (2, 2)
 	rotate 0.2
	drawHero
~~~

We apply a rotation now all our coordinates are transformed.
Note that angle of rotation is set with taus. The tau is relative
measure of rotation. 1 means full circle 0.5 is half of the circle and so on. 


### Local transformations

What if we want to draw another hero that watches as the first flies. 
Here is the first attempt:

~~~Haskell
draw () = do
	background (grey 230)
	
 	translate center
 	scale (2, 2)
 	rotate 0.2
	drawHero

	translate (100, 320)
	scale (0.5, 0.5)
	drawHero
~~~

But we can not see the second hero. The root of the problem is that transformations
accumulate. We'd like to through away the transformations for the first hero after it's drawn
on the screen. We'd like to start afresh for the second hero. 

To solve this problem there is a function `local`. It introduces the scope of transformation:

~~~Haskell
draw () = do
	background (grey 230)
	local $ do
	 	translate center
	 	scale (2, 2)
	 	rotate 0.2
		drawHero
	local $ do 
		translate (100, 320)
		scale (0.5, 0.5)
		drawHero
~~~

Let's look at it's signature:

~~~Haskell
local :: Pio () -> Pio ()
~~~

It takes in a procedure and executes it in the local scope of transformations.
Then it falls back to the previous space.

Exercise: draw a crowd of heroes.

