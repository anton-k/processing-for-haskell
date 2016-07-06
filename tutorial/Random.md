Randomness
================================

Let's create some random stuff! We have several functions to simulate randomness.
The simplest functions are `random` and it's bro `random2`. They create random values 
from the given interval:

~~~Haskell
random :: Float -> Pio Float
random maxValue = random (0, maxValue)

random2 :: (Float, Float) -> Pio Float
random2 (minValue, maxValue) = ...
~~~

Let's create a chaotic movement. This type of random behavior is called brownian motion:

~~~Haskell
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update }

width = 400
height = 400
center = (0.5 * width, 0.5 * height)

setup = do
	size (width, height)
	background (grey 0)	
	stroke (grey 255)
	strokeWeight 2
	frameRate 30
	return (center, center)

draw (p1, p2) = do
	line p1 p2

len = 5

update (_, p) = do
	x <- random2 (-len, len)
	y <- random2 (-len, len)
	return (p, p + (x, y))
~~~	

We have functions to create ranfom points on the screen and colors: `randomP2`, `randomCol`, `ranodimCola` (color with transparency or alpha).
Let's draw colored circles at random on the screen:

~~~Haskell
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width = 400
height = 400
center = (0.5 * width, 0.5 * height)

setup = do
	size (width, height)
	background (grey 255)		
	frameRate 7

draw () = do
	drawRandomCircle

drawRandomCircle = do	
	noStroke
	fill =<< randomCola
	rad <- random2 (10, 40)
	circle rad =<< randomP2
~~~


Gaussian noise
------------------------------------------

With gaussian noise we can create values that most frequently fall to a certain value
but often miss it by some degree. With this change we can spread all our circles around the center:


~~~Haskell
drawRandomGaussCircle = do	
	noStroke
	fill =<< randomCola
	rad <- random2 (10, 20)
	x <- randomGaussian
	y <- randomGaussian
	circle rad  (50 * (x, y) + center)
~~~

The function `randomGaussian` generates values that are spread around zero. Amount of spread equals to 1.
We can add th value to change the center point and multiply the value to  change the amount of spread.

Perlin noise
-------------------------------------------

(right now it looks like haskell implementation of Perlin noise is not accurate)

Sometimes the random is way too `random` and `randomGaussian` is too focused. 
We can use the `noise` function to create "organic" chaotic structures.
It implements the Perlin noise.

The `noise` is a function that maps values to random numbers. We can control the amount
of spread by proximity of values of the argument. If values are close to each other then 
amount of noise is also small. It means that `noise` is a continuous function.

We have 1D, 2D and 3D noise functions:

~~~Haskell
noise1 :: Float -> Pio Float
noise2 :: P2 -> Pio Float
noise2 :: P3 -> Pio Float
~~~

Let's divide the screen in two halves with natural looking line:

~~~Haskell
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width = 400
height = 400

setup = do
	size (width, height)	
	forM [0 .. width] $ \x -> do
		y <- noise1 (x / 100)
		return (x, (30 * y - 15) + 0.5 * height)
		
draw ps = do
	linePath ps		
~~~

Let's create a texture:

~~~Haskell
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width = 400
height = 400

setup = do
	size (width, height)	
	noStroke
	forM [(x, y) | x <- [0, 5 .. width], y <- [0, 7 .. height]] $ \(x, y) -> do
		z <- noise2 (x / 100, y / 100)
		return ((x, y), 255 * z)
		
draw ps = do
	forM_ ps $ \(p, col) -> do
		fill (grey col)
		circle 5 p
~~~

Predictable noise
----------------------------		

We can set the seed for random generators:

~~~Haskell
noiseSeed, randomSeed :: Float -> Pio ()
~~~

With fixed seed we can create reproducible noise behaviors.
The `randomSeed` controls the random and gaussian noises at the same time.
