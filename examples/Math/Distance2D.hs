-- original code: https://processing.org/examples/distance2d.html

-- Distance 2D.
-- 
-- Move the mouse across the image to obscure and reveal the matrix. 
-- Measures the distance from the mouse to each square and sets the 
-- size proportionally.
 
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

thin = 8
thick = 36

setup = do    
  size (width, height)
  noStroke
  fill (grey 255)

maxDistance = distance (0, 0) (width, height)

draw () = do
	background (grey 0)
	m <- mouse
	forM_ [0, 20 .. width] $ \i -> do
		forM_ [0, 20 .. height] $ \j -> do
			let s = distance m (i, j) / maxDistance * 66			
			ellipse (i, j) (s, s)

----------------------------------------
-- Side note
--
-- We use the function `distance` that is re-exported with vector-space package.

