
The first steps. The structure of animation
=========================================

The animation is some evolving state that we can render on the screen.
We have initial state, the function that can draw the state and the function that updates the state.
So there are three main functions:

* `setup` -- it produces initial state and sets up the scene. Here we can define the window size, frame rate etc.

* `draw` -- it turns the state to picture. It renders or model on the screen.

* `update` -- it calculates the next state out of current one.

The functions have following signatures:

~~~Haskell
setup  :: Pio st

draw   :: st -> Pio ()

update :: st -> Pio st
~~~

The `Pio` is IO-monad augmented with processing functions. We can also lift plain old IO-actions
to `Pio` with the function `liftIO`.

Let's draw a simple static picture. We are going to draw a circle in the center of the window.
We are going to define our three main functions and render the animation. 

~~~Haskell
import Graphics.Proc

width  = 400
height = 400
center = (width / 2, height / 2)

setup = do
	size (width, height)
	return ()

draw () = do
	background (grey 0)
	fill (grey 255)
	circle 17 center	

update x = return x

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update }
~~~

In setup we set the sizes of the window and return the initial state.
Our picture is static so the state is not going to change that is why we return 
a value of the unit type. It has only one value.

The function `draw` clears the picture with black color (`grey 0`), sets the
current color to white (`fill (grey 255)`) and draws a circle in the center of the screen.

The function `update` doesn't change anything it just passes the state around.

We are ready to render our static animation. To do that we set the three callbacks 
and pass them to the function `runProc`.

~~~Haskell
runProc :: Proc -> IO ()
~~~

The data type `Proc` contains all callback functions. There are many callbacks but right now 
we can set only three of them. We use the default value `def` and redefine the fields we need:

~~~Haskell
main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update }
~~~

Save the code to file and execute runhaskell on it:

~~~
> runhaskell Static.hs
~~~

We have a nice picture of the lonely star. Let's create a planet that spins around our Sun!
The planet has parameters:

~~~Haskell
data Planet = Planet 
	{ planetColor 	:: Col
	, planetRadius :: Float
	, planetAngle 	:: Float
	, planetSpeed 	:: Float
	, planetDistanceToSun :: Float }
~~~

A planet can have color, radius (or size), current angle on the orbit, speed of rotation
and distance to the Sun. Let's define the function to draw the planet:

~~~Haskell
drawPlanet p = local $ do
	translate center
	rotate (planetAngle p)
	noStroke
	fill (planetColor p)
	circle (planetRadius p) (0, planetDistanceToSun p)
~~~

We draw the planet at transformed coordinates. First we translate the coordinates
so that the point (0, 0) corresponds to the center of the screen. Then we rotate
the space by the planet's angle. We are going to update the angle of rotation so that
the planet could move around the Sun. Let's implement this function:

~~~Haskell
updatePlanet p = p { planetAngle = angle + speed }
	where
		angle = planetAngle p
		speed = planetSpeed p
~~~

We can define the initial state:

~~~Haskell
initPlanets = [Planet green 10 0 0.0013 85, Planet red 8 0 0.001 155]
~~~

Here is the complete code sample. Notice how easy it is to add new planets.
We can just put them into init list and system will take of them:

~~~Haskell
import Graphics.Proc

width  = 400
height = 400
center = (width / 2, height / 2)

setup = do
	size (width, height)
	return initPlanets

draw xs = do
	background (grey 0)
	drawSun
	mapM_ drawPlanet xs

update xs = return $ fmap updatePlanet xs

-------------------------------------
-- sun

drawSun = do
	fill (grey 255)
	circle 17 center	

-------------------------------------
-- planets

data Planet = Planet 
	{ planetColor 	:: Col
	, planetRadius :: Float
	, planetAngle 	:: Float
	, planetSpeed 	:: Float
	, planetDistanceToSun :: Float }

drawPlanet p = local $ do
	translate center
	rotate (planetAngle p)
	noStroke
	fill (planetColor p)
	circle (planetRadius p) (0, planetDistanceToSun p)

updatePlanet p = p { planetAngle = angle + speed }
	where
		angle = planetAngle p
		speed = planetSpeed p

initPlanets = [Planet green 10 0 0.0013 85, Planet red 8 0 0.001 155]

-------------------------------------

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update }
~~~

We use standard Haskell functions to update all planets in the list.
The `mapM_` to draw planets and accumulate the effects and `fmap`
to transform all elements in the list.

Exercise: try to add satellites.


