-- original code: https://processing.org/examples/constrain.html

-- Easing.
-- 
-- Move the mouse across the screen and the symbol will follow. 
-- Between drawing each frame of the animation, the program 
-- calculates the difference between the position of the symbol 
-- and the cursor. If the distance is larger than 1 pixel, the symbol 
-- moves part of the distance (0.05) from its current position toward the cursor.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }
 
width  = 640
height = 360

sizes = (width, height)
center = 0.5 *^ sizes
easing = 0.0065
edge   = 100
radius = 24
inner  = edge + radius

setup = do
  size (width, height)
  ellipseMode Radius
  rectMode Corners
  noStroke
  return center

draw pos = do
  background (grey 51)
  fill (grey 76)
  rect (edge, edge) (width-edge, height-edge)
  fill (grey 255)
  ellipse pos (radius, radius)

update pos = do
  m <- mouse
  return (constrain2 (vin, sizes - vin) (pos + easing *^ (m - pos)))
  where 
  	vin = (inner, inner)
