-- the original example:
--
-- https://processing.org/examples/coordinates.html
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  -- Sets the screen to be 640 pixels wide and 360 pixels high
  size (P2 width height)

draw () = do
  -- Set the background to black and turn off the fill color
  background (grey 0)
  noFill

  -- The parameter of the point function specifies coordinates.
  -- It's a pair of floats (x and y coordinate).
  stroke (grey 255)
  point (P2 (width * 0.5) (height * 0.5))
  point (P2 (width * 0.5) (height * 0.25))

  -- Coordinates are used for drawing all shapes, not just points.
  -- Parameters for different functions are used for different purposes.
  -- For example, the first parameter to line() specifies
  -- the coordinates of the first endpoint and the second prameter
  -- specifies the second endpoint
  stroke (rgb 0 153 255)
  line (P2 0 (height * 0.33)) (P2 width (height * 0.33))


  -- By default, the first two parameters to rect() are the
  -- coordinates of the upper-left corner and the second pair
  -- is the width and height
  stroke (rgb 255 153 0)
  rect (P2 (width*0.25) (height*0.1)) (P2 (width * 0.5) (height * 0.8))


---------------------------------------------
-- Sidenotes
--
-- notice the usage of width and height constants.
-- They have the different meaning from the values width and height
-- in processing.
--
-- Haskell names for processing width and height are winWidth and winHeight.
-- But it's not so convenient to use as in Processing due to implicit sideeffect.
-- Haskell makes that side effect explicit. So we should write
--
-- > do
-- >   w <- winWidth
-- >   h <- winHeight
--
-- But we can work it around with a simple trick. We just define constants width and height
-- and then use them to set the sizes of the window:
--
-- > setup = do
-- >   size (width, height)
--
-- Then we can use them just like in processing. Because they are pure constant values.


