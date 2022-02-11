-- Noise1D.
--
-- Using 1D Perlin Noise to assign location.

-- todo check out the Perlin noise implementation. I'm not sure that init params are right
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

xincrement = 0.01

setup = do
  size (P2 width height)
  noStroke
  background (grey 0)
  return xoff
  where xoff = 0.0

draw xoff = do
  fill (greya 0 10)
  rect 0 (P2 width height)
  n <- fmap (* width) $ noise1 xoff
  fill (grey 200)
  ellipse (P2 n (height/2)) 64

update xoff = return (xoff + xincrement)
