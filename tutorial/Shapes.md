Drawing shapes and curves
====================================

There are some other forms beside the circle available to us.
Let's draw them on the screen:

~~~Haskell
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

setup = do
	size (400, 400)	

draw () = do
	noStroke
	fill (rgb 255 145 23)
	ellipse (50, 50) (40, 70)
	
	noStroke
	fill (grey 74)
	rect (150, 50) (60, 90)

	strokeWeight 6
	stroke (rgb 24 146 75)
	line (50, 200) (350, 200)

	noStroke
	fill (greya 34 140)
	rect (100, 300) (75, 75)

	noStroke
	fill (rgba 230 53 38 100)
	triangle (50, 350) (100, 250) (150, 350)

	strokeWeight 2
	stroke purple
	bezier (250, 250) (350, 200) (400, 350) (367, 80)

	fill green
	stroke navy
	quad (220, 300) (270, 350) (320, 300) (260, 260)

	stroke black
	point (350, 230)

	linePath [(10, 10), (10, 390), (390, 390)]
~~~

Coordinates of the shapes are given with vectors which contain (x, y)-coordinates.
You can see various shapes and ways to set the colors.
We set the colors with two functions:

~~~Haskell
fill, stroke :: Col -> Pio ()
~~~

The `fill` sets the color of the body of the shape and `stroke` sets the color of the rim.
We can supress the drawing of the body or the rim with commands:

~~~Haskell
noFill, noStroke :: Pio ()
~~~

Notice the various ways to specify the colors. Arguments range in the interval `(0, 255)`:

* `rgb` -- creates colors out of red, green and blue.

* `grey` -- creates grey colors out of singl component.

* `rgba` -- creates RGB-color with fourth parameter for transparency or **a**lpha.

* `greya` -- creates grey colors with transparency as a second argument.

There are predefined colors: `red`, `green`, `blue`, `orange`, `olive` etc.

The function: `setAlpha` can change the trasparency of the given color.
