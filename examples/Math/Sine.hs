-- original code: https://processing.org/examples/sine.html

-- Sine.
--
-- Smoothly scaling size with the sin() function.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

diameter = height - 10

setup = do
  size (P2 width height)
  noStroke
  fill (rgb 255 204 0)
  return angle
  where angle = 0

draw angle = do
  background (grey 0)
  circle d1 (P2 0 (height/2))
  circle d2 (P2 (width/2) (height/2))
  circle d3 (P2 width (height/2))
  where
    d1 = 0.5 * (10 + (sin(angle) * diameter/2) + diameter/2)
    d2 = 0.5 * (10 + (sin(angle + pi/2) * diameter/2) + diameter/2)
    d3 = 0.5 * (10 + (sin(angle + pi) * diameter/2) + diameter/2)

update angle = return (angle + 0.03)
