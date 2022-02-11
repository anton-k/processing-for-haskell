-- original code: https://processing.org/examples/easing.html

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

center = 0.5 *^ (P2 width height)
easing = 0.05

setup = do
  size (P2 width height)
  noStroke
  return center

draw pos = do
  background (grey 51)
  fill (grey 255)
  ellipse pos 66

update pos = do
  m <- mouse
  return (pos + easing *^ (m - pos))
