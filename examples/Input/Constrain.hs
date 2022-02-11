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

sizes = (P2 width height)
center = 0.5 *^ sizes
easing = 0.06
edge   = 100
radius = 24
inner  = edge + radius

setup = do
  size sizes
  ellipseMode Radius
  rectMode Corners
  noStroke
  return center

draw pos = do
  background (grey 51)
  fill (grey 76)
  rect (P2 edge edge) (P2 (width-edge) (height-edge))
  fill (grey 255)
  ellipse pos (P2 radius radius)

update pos = do
  m <- mouse
  return (constrain2 (vin, sizes - vin) (pos + easing *^ (m - pos)))
  where
  	vin = (P2 inner inner)
