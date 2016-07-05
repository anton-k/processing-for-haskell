User input
=============================

If animation can react to user input we often tend to call it a Game.
We can react to mouse motion and clicks and to keyboard strokes.
To do it we declare a special callbacks just like we did it to update and draw the state.

Mouse clicks
------------------------------------

There are two main callbacks:

* `mousePressed` -- it's invoked when mouse button is pressed

* `mouseReleased` -- it's invoked when mouse button is released

Functions have the same signature as function `update`.
In the `Proc` data type we should declare them with prefix `proc`.

Let's recall the example with solar system (see the "first steps" section).
We can change orientation of rotation with mouse clicks like this.
We ave to add another parameter to state:

We should update the setup function:

~~~Haskell
setup = do
	size (width, height)
	return (1, initPlanets)
~~~

We are going to multiply the angle shift by orientation factor (the first element in the pair).
If it equals to -1 the orientation of rotation changes:

~~~~Haskell
update (orient, xs) = return $ (orient, fmap (updatePlanet orient) xs)

updatePlanet orient p = p { planetAngle = angle + orient * speed }
	where
		angle = planetAngle p
		speed = planetSpeed p
~~~~

Notice the `orient` argument. Also we need to update the usage of arguments in the draw function:

~~~Haskell
draw (_, xs) = do
	background (grey 0)
	drawSun
	mapM_ drawPlanet xs
~~~

We ignore the orientation during drawing. Here is how we can update the orientation:

~~~Haskell
mousePressed (orient, ps) = return (negate orient, ps)
~~~


And we should add this callback to the main procedure:

~~~Haskell
main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update
	, procMousePressed = mousePressed }
~~~

The full code you can find in the directory code, see the `PlanetClick.hs`.

Mouse motion
--------------------------------

We can read the current position of the mouse with the function `mouse`.
Also there are functions to read individual coordinates called `mouseX` and `mouseY`.

A trivial example:

~~~Haskell
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width  = 400
height = 400

setup = do
	size (width, height)

draw () = do
	background (grey 12)	
	m <- mouse
	circle 15 m
~~~

It draws a circle that follow the mouse.

Mouse buttons
--------------------------------------------

We can distinguish which button was pressed with the function `mouseButton`.
It returns the value of type `MouseButton`. The value is wrapped in `Maybe`.
If nothing was pressed then `Nothing` is returned.

We can create a routine that draws lines. The state of the animation
is a list of points. If we click on the left mouse button we store a current point in 
the list if we press the right button we remove the last point from the list.

~~~Haskell
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procMousePressed = mousePressed }

width = 400
height = 400

setup = do
	size (width, height)
	strokeWeight 2
	stroke (grey 255)
	return []

draw ps = do
	background (grey 0)
	case ps of
		[] -> return ()
		_  -> do
			m <- mouse
			linePath (m : ps)

mousePressed ps = do
	mb <- mouseButton
	case mb of
		Just LeftButton -> do
			m <- mouse
			return (m : ps)
		Just RightButton -> do
			if null ps 
				then return []
				else return (tail ps)
		_ -> return ps
~~~

If we press left button we add the point to the list:

~~~Haskell
	case mb of
		Just LeftButton -> do
			m <- mouse
			return (m : ps)
~~~

If we press the right button we remove the point from the list:

~~~Haskell
		Just RightButton -> do
			if null ps 
				then return []
				else return (tail ps)
~~~

We check the list for emptiness to avoid exceptions.

Exercise: provide the user ability to write loops. 
If user clicks very close to the point that lies at the beginning of the 
line the program should close the line-loop and let the user to draw 
the next chain of lines.

Keyboard
------------------------------------

We can react on keyboard events with callbacks `keyPressed` and `keyReleased`.
We can read the last pressed key value with function `key`:

~~~Haskell
key :: Pio Key
~~~

The dta type `Key` is borrowed from the haskell GLUT library just like the type `MouseButton`.
Let's draw a circle and let the user to move it with standard for old-school gaming
buttons `wasd`. `w` -- for up, `a` - go left, `s` -- go down and `d` -- go right:


~~~
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procKeyPressed = keyPressed }

width  = 400
height = 400
center = (0.5 * width, 0.5 * height)

setup = do
	size (width, height)
	return center

draw p = do
	background (grey 12)	
	fill (grey 255)	
	circle 15 p

dt = 5

keyPressed (x, y) = do 
	k <- key
	return $ case k of
		Char 'w'  -> (x, y - dt)
		Char 'a'  -> (x - dt, y)
		Char 's'  -> (x, y + dt)
		Char 'd'  -> (x + dt, y)
~~~

We can do the same thing with arrows:

~~~Haskell
		SpecialKey KeyUp    -> (x, y - dt)
		SpecialKey KeyLeft  -> (x - dt, y)
		SpecialKey KeyDown  -> (x, y + dt)
		SpecialKey KeyRight -> (x + dt, y)
~~~

Exercise: Now we can write simple game!
hint see the library function `frameRate` to set the rate of the animation.
Also time reading functions can be useful for writing games.


