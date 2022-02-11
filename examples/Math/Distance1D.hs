-- original code: https://processing.org/examples/distance1d.html

-- Distance 1D.
--
-- Move the mouse left and right to control the speed and direction of the moving shapes.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

thin = 8
thick = 36

setup = do
  size (P2 width height)
  noStroke
  return (xpos1, xpos2, xpos3, xpos4)
  where
    xpos1 = width/2
    xpos2 = width/2
    xpos3 = width/2
    xpos4 = width/2

draw (xpos1, xpos2, xpos3, xpos4) = do
  background (grey 0)

  fill (grey 102)
  rect (P2 xpos2 0) (P2 thick (height/2))
  fill (grey 204)
  rect (P2 xpos1 0) (P2 thin (height/2))
  fill (grey 102)
  rect (P2 xpos4 (height/2)) (P2 thick (height/2))
  fill (grey 204)
  rect (P2 xpos3 (height/2)) (P2 thin (height/2))


update (xpos1, xpos2, xpos3, xpos4) = do
  mX <- mouseX
  let mx = mX * 0.4 - width / 5
      xpos1' = withinBounds (xpos1 + coeff * mx/16)
      xpos2' = withinBounds (xpos2 + coeff * mx/64)
      xpos3' = withinBounds (xpos3 -  coeff * mx/16)
      xpos4' = withinBounds (xpos4 - coeff * mx/64)
  return (xpos1', xpos2', xpos3', xpos4')
  where
    withinBounds value =
       if (value < -thin)
          then width
          else
            if (value > width)
              then -thin
              else value
    coeff = 0.6
