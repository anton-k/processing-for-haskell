-- original code: https://processing.org/examples/regularpolygon.html

-- Regular Polygon
--
-- What is your favorite? Pentagon? Hexagon? Heptagon? No? What about the icosagon? 
-- The polygon() function created for this example is capable of drawing any regular polygon. 
-- Try placing different numbers into the polygon() function calls within draw() to explore.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (width, height);

draw() = do
  background (grey 102)
  
  drawPoly (width*0.2, height*0.5) (1 / 200) 82 3   -- Triangle
  drawPoly (width*0.5, height*0.5) (1 / 50)  80 20  -- Icosahedron
  drawPoly (width*0.8, height*0.5) (-(1/ 100)) 70 7 -- Heptagon

drawPoly center speed radius npoints = local $ do
  translate center
  n <- frameCount
  rotate (float n * speed);
  poly (0, 0) radius npoints

poly center radius npoints = 
  polygon $ fmap (onCircle radius center) [0, 1/npoints .. 1]
