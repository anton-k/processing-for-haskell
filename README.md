Processing for Haskell
=============================================

Computer Graphics for kids and artists! The library implements
Processing language in Haskell.

Welll, But actually it...

... implemets a **subset** of Processing Language in Haskell. So ...


The project needs your help
-------------------------------------------------------------

The Processing being a small language implements some  tons of
magic with OpenGL  under the hood. There are really big implementations
of graphics primitives. I can not finish this thing all alone.
So of you are really interested in seing the package finished.
If you want all features of Processing be implemented please **do** contribute!


Guide 
--------------------------------------------------------------

If you are familiar with processing you probably know that
there are two main functions which set the picture into motion.
They are `setup` and `draw`. 

The function:

* `setup` initializes the window scene and state of the animation.

* `draw` is for redrawing the animation and updating the state.

We are going to draw the Sun and a planet that spins around the Sun.
The typical processing code may look like this:

~~~Java
// constants
float rad = 55;
float winWidth = 300;
float winHeight = 300;
float centerX = winWidth / 2;
float centerY = winHeight / 2;

// state
float t;

// standard callbacks

void setup() {
  size(winWidth, winHeight);  
  t = 0;  
}

void draw() {  
  background(255);
  drawSun();
  drawPlanet();
  updateState();  
}

// user defined functions

void drawSun() {  
  fill(0);
  ellipse(centerX, centerY, 30, 30);
}

void drawPlanet() {   
  float x = centerX + rad * cos(t);
  float y = centerY + rad * sin(t);
  
  fill(145);
  ellipse(x, y, 12, 12);
}

void updateState() {
   t += 0.025;  
}
~~~

If you know the Java you already know the syntax of Processing.
The Processing uses simplified version of Java syntax 
(no need for many boilerplate keywords, ability to write top-level functions).

Let's read this code by blocks. The first block of code defines the constant parameters.
They stay fixed for the lifetime of the program:

~~~Java
// constants
float rad = 55;
float winWidth = 300;
float winHeight = 300;
float centerX = winWidth / 2;
float centerY = winHeight / 2;
~~~

The next thing is to define a variable for the state:

~~~Java
// state
float t;
~~~

Then we set the window sizes and state:

~~~Java
void setup() {
  size(winWidth, winHeight);  
  t = 0;  
}
~~~

The `setup` is keyword. The user should define this function to initialize the state.

We are ready to draw the picture of our universe on the screen:

~~~Java
void draw() {  
  background(255);
  drawSun();
  drawPlanet();
  updateState();  
}
~~~

The first command clears the background with white color. 
The color values are measured in values from the interval 0 to 255.
The `draw` is also a keyword. 

Then we invoke user-defined functions to draw the sun, planet and to update the state.

Let's draw the Sun:

~~~Java
void drawSun() {  
  fill(0);
  ellipse(centerX, centerY, 30, 30);
}
~~~

The first command (`fill`) sets the fill color for all following shapes that we are going to draw.
We set it to black. The next command draws a centered circle. The planet is more interesting, because
it's going to move:

~~~Java
void drawPlanet() {   
  float x = centerX + rad * cos(t);
  float y = centerY + rad * sin(t);
  
  fill(145);
  ellipse(x, y, 12, 12);
}
~~~

There is a bit of trigonometry going on. We just calculate the position of planet 
that depends on the state `t`. The `t` is the angle of rotation. The next steps are similar 
to the drawing of the Sun. We draw a circle that is a bit smaller. 

The last step is to update the state so that with every new frame the angle increases:

~~~Java
void updateState() {
   t += 0.025;  
}
~~~

The code is complete. We can hit the run button in the Processing environment 
and it will show us the tiny movie.


Let's discuss how to write this program in Haskell with our library. 
I've tried to make the library as close to Processing as possible, but
there are some tricks that can not be done in Haskell. The processing is imperative
or object oriented language and Haskell is purely functional. So some stuff like
access to global variables is not available in Haskell. But we will find the way out of it.
I promise! So read on.

Let's start with importing our library:

~~~Haskell
import Graphics.Proc
~~~

Then we can define a block of constants:

~~~Haskell
-- constants
rad       = 55
winWidth  = 300
winHeight = 300
centerX   = winWidth / 2
centerY   = winHeight / 2
~~~

The code is quite the same. No surprises here.

The next thing is to define a state and initialize it with setup.
With Java it was a simple global variable definition. 
But we can not use globals in Haskell. The trick is to augment 
our standard functions `setup` and `draw` with state passing.
In the Java or Processing code the `setup` has no arguments
and produced nothing. Java folks are used to this way of 
state manipulation but we are going to do it going functional way.

Our `setup` function is going to produce initial state value:

~~~Haaskell
setup :: Pio Float
setup = do
	size (winWidth, winHeight)
	return 0
~~~

With the first command (`size`) we set the sizes of the windows.
With second command we return the value of our state. Later we are going
to pass it as an argument to draw `function`. Notice the type signature of the function.
The value is wrapped is `Pio`. The `Pio` is short for Processing IO-monad. 
That's familiar to Haskellers IO-monad that is augmented with Processing features
(drawing primitives, noise generators, time queries and so on). 

So the bottom line is that haskell `setup` function should produce a state
wrapped in special case of IO-monad.

Let's draw everything:

~~~Haskell
draw :: Float -> Draw
draw t = do
	background (gray 255)
	drawSun
	drawPlanet t
~~~

We do the same things we did in Java. but now we get the state as an argument
and we pass it to the function `drawPlanet` that is going to need the state.
Also notice that there is no state update. We are going to do it with the separate function.

Notice the type `Draw`. It's just an alias for `Pio ()`.

There is a slight difference in color handling. 
The function `gray` constructs RGB-color out of single value. Why do we need that? 
In Processing like in Java we can define several functions with the same name. They are
distinguished with type-signatures of the arguments. But Haskell is more restrictive. 
We should have only one function with the given name. So Processing `background` function
can take one or three arguments. If it has only one it constructs the gray color if it has three
it uses them as red, green and blue parameters of the color. In Haskell we use the function
`gray` to construct the gray colors and `rgb` to construct simple colors. 
Also there are functions `greya` and `rgba` they have another one argument for **a**lpha or transparency.

Let's draw the sun and the planet:

~~~Haskell
drawSun = do
	fill (grey 0)
	ellipse (centerX, centerY) (30, 30)
~~~

The Sun is static so we don't need any input.
Notice the difference of `ellipse` function. In Haskell I've decided to
make the points are default. The Processing always uses a plain float values.
It's ok for introduction but I often find that the point type (or representation with pairs of numbers)
is much more convenient. 

Let's draw a planet:

~~~Haskell
drawPlanet t = do
	fill (grey 145)
	ellipse (x, y) (12, 12)
	where
		x = centerX + rad * cos t
		y = centerY + rad * sin t
~~~

You can see that in Haskell we pass the state into the function in order to use it.
The trigonometry stuff goes on here and we draw it in the same manner as the Sun.

The cool thing about using points in place of numbers is that in Haskell we have 
numeric  instances for points. We can rewrite the code like this:

~~~Haskell
winSizes = (300, 300)
center   = 0.5 *^ winSizes

drawPlanet t = do
	fill (grey 145)
	ellipse p 12
	where
		p = center + rad *^ (cos t, sin t)
~~~

The operator `*^` multiplies both values o the pair with the given value.
So it multiplies a float value by point or scales the point with the value.
The `+` and numeric literals are `overloaded` for points. We can some them up
and a numeric value `12` produces a pair of `(12, 12)`. So the formula becomes
a single line definition:

~~~Haskell
p = center + rad *^ (cos t, sin t)
~~~

We are ready to update the state:

~~~Haskell
update :: Float -> Pio Float
update t = return (t + 0.025)
~~~

It's much the same thing we did, but now the state is explicitly stated in 
the type signature of the function.

So we can set the things in motion! But the Haskell language knows nothing about
special meaning that we put into the names `setup` and `draw`. We need to give it a hint!
To run the standard processing callback functions we use the function `runProc`:

~~~Haskell
main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update 
	}
~~~

The function `runProc` takes in a `Proc` data structure. The `Proc` contains
all callbacks that we are going to use. Many callbacks are specified with default 
values. To specify only part of callbacks that we are going to use we use the trick 
with Haskell default values. The Proc has instance of class `Data.Default`. 
This class has only one method:

~~~Haskell
class Default a where
	def = a
~~~

So the `def` contains all callback we need. Then we can set the callbacks we need with our functions:

~~~
def { procSetup  = setup
	, procDraw   = draw
	, procUpdate = update 
	}
~~~

and pass this value to the `runProc` function.

Here is the complete code for Haskell program:

~~~
import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update 
	}

-- constants

rad = 55
sizes  = (300, 300)
center = 0.5 *^ sizes

-- standard functions

setup :: Pio Float
setup = do
	size sizes
	return 0

draw :: Float -> Draw
draw t = do
	background (grey 255)
	drawSun
	drawPlanet t

update :: Float -> Pio Float
update t = return (t + 0.0025)

-- drawing

drawSun = do
	fill (grey 0)
	ellipse center 30

drawPlanet t = do
	fill (grey 145)
	ellipse p 12
	where	
		p = center + rad *^ (cos t, sin t)	
~~~

The code is almost the same as Processing code but there are differences.
Let's briefly recall all the differences:

* In Processing we can use global variables to update the state.
	In Haskell we explicitly pass the state and update it. 
	Standard callbacks take in state as an argument or pass it as a result.

	The `setup` function produces initial state. The `draw` takes in state as an argument.
	The function `update` takes in the state and produces the new value.

* In Haskell the state update is separated from drawing process.
	We have two function `draw` and `update`:

	~~~Haskell
	draw   :: st -> Draw
	update :: st -> Pio st
	~~~

* We have a special monad `Pio`, that augments the IO-monad with processing functionality.
	We can use `liftIO` function to turn IO-actions to `Pio`s:

	~~~Haskell
	text <- liftIO (readFile "file.txt")
	~~~

	There is a handy alias:

	~~~Haskell
	type Draw = Pio ()
	~~~

* Processing has standard names for drawing and setup functions.
    In Haskell we should use the `runProc` function to run the animation
    and specify the callbacks in the special data structure `Proc`.

* Processing often uses simple numbers where point type can be more appropriate. 
	So in Processing we draw a line with four arguments:

	~~~Java
	line(x1, y1, x2, y2)
	~~~

	But in Haskell two points is enough:

	~~~Haskell
	let p1 = (x1, y1)
	    p2 = (x2, y2)
	line p1 p2
	~~~

* We use special functions to construct colors:

	~~~Haskell
	grey  :: Float -> Col
	greya :: Float -> Float -> Col
	rgb   :: Float -> Float -> Float -> Col
	rgba  :: Float -> Float -> Float -> Float -> Col

	fill   		:: Col -> Draw
	stroke 		:: Col -> Draw
	background 	:: Col -> Draw
	~~~

**TODO**
