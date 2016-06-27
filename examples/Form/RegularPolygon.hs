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
  smooth
  size (width, height)

draw() = do
  background (grey 102)
  fill (grey 250)
  
  drawPoly (width*0.2, height*0.5) (1 / 400) 82 3   -- Triangle
  drawPoly (width*0.5, height*0.5) (1 / 100)  80 20  -- Icosahedron
  drawPoly (width*0.8, height*0.5) (-(1/ 200)) 70 7 -- Heptagon

-- the function `local` is equivalent of pair of pushMatrix and popMatrix.
-- We specify the scope of local transformation with indentation.
drawPoly center speed radius npoints = local $ do
  translate center
  n <- frameCount
  rotate (float n * speed);
  poly (0, 0) radius npoints

poly center radius npoints = 
  polygon $ fmap (onCircle radius center) [0, 1/npoints .. 1]

-------------------------------------------
-- Sidenote
--
-- In processing one often uses the trick with putting the matrix on stack
-- and the puting it out of the stack to emulate the local transformations.
-- Code may look like this:
--
-- > pushMatrix();
-- > stmt1;
-- > stmt2;
-- > ...
-- > stmtN;
-- > popMatrix();
-- 
-- In Haskell there is much better way to express the same idea. we can just write:
--
-- > local $ do
-- >     stmt1
-- >     stmt2
-- >     ...
-- >     stmtN
--
-- Compiler knows which statments to enclose in local transformation by indentation.
-- The function local takes a block of code and executes it its own matrix frame of space transformation.
--
-- Another thing to note is misc function `onCircle`. It maps an interval [0, 1] to the circle. 
--
-- > onCircle :: Radius -> Center -> float -> P2
--
-- So the first two arguments specify the shape of a circle and the last argument is a point on the circle.
-- It's very convinient way to put points n the given circle.
--
-- The function polygon constructs a polygon out of list of points. The polygon is olway convex.
-- So let's take a closer look at this rather dence haskell statment:
--
-- > polygon $ fmap (onCircle radius center) [0, 1/npoints .. 1]
--
-- first we create a list of `npoints` number of floats that occupy the interval [0, 1].
-- Then we map interval to circle: 
--
-- > fmap (onCircle radius center) [0, 1/npoints .. 1]
--
-- and with final function in the chain `polygon` we create a polygon.
-- Unfortunately generic shape constructors right now are not implemented.
-- So there is no beginShape and endShape functions.
