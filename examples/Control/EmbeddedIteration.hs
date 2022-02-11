-- original code: https://processing.org/examples/embeddediteration.html

-- Embedding Iteration.
--
-- Embedding "for" structures allows repetition in two dimensions. *

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (P2 width height)

gridSize = 40
center = 0.5 *^ (P2 width height) -- special operator to scale vector by float

draw () = do
	background (grey 0)
	forM_ [gridSize, 2*gridSize .. width - gridSize] $ \x -> do
		forM_ [gridSize, 2*gridSize .. height - gridSize] $ \y -> do
			noStroke
			fill (grey 255)
			rect (P2 (x - 1) (y - 1)) (P2 3 3)
			stroke (greya 255 100)
			line (P2 x y) center
