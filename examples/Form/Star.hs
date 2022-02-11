-- original code: https://processing.org/examples/star.html

-- Star
--
-- The star() function created for this example is capable of drawing
-- a wide range of different forms. Try placing different numbers into
-- the star() function calls within draw() to explore.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  smooth
  size (P2 width height)

draw() = do
  background (grey 102)
  fill (grey 250)

  drawPoly (P2 (width*0.2) (height*0.5)) (1 / 200) 70 3   -- Triangle
  drawPoly (P2 (width*0.5) (height*0.5)) (1 / 400)  100 40  -- Icosahedron
  drawPoly (P2 (width*0.8) (height*0.5)) (-(1/ 100)) 70 5 -- Heptagon

-- the function `local` is equivalent of pair of pushMatrix and popMatrix.
-- We specify the scope of local transformation with indentation.
drawPoly center speed radius npoints = local $ do
  translate center
  n <- frameCount
  rotate (float n * speed);
  poly (P2 0 0) radius npoints

poly center radius npoints =
  polygon $ fmap (onCircle radius center) [0, 1/npoints .. 1]

star center radius npoints = undefined
-- not convex polygons are not implemented yet (todo)
