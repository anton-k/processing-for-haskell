-- the original example:
--
-- https://processing.org/examples/shapeprimitives.html

-- Shape Primitives.
--
-- The basic shape primitive functions are triangle(), rect(), quad(), ellipse(), and arc().
-- Squares are made with rect() and circles are made with ellipse().
-- Each of these functions requires a number of parameters to determine the shape's position and size.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
	size (P2 width height)
	noStroke

draw () = do
	background (grey 0)

	fill (grey 204)
	triangle (P2 18 18) (P2 18 360) (P2 81 360)

	fill (grey 102)
	rect (P2 81 81) (P2 63 63)

	fill (grey 204);
	quad (P2 189 18) (P2 216 18) (P2 216 360) (P2 144 360)

	fill (grey 255)
	ellipse (P2 252 144) (P2 72 72)

	fill (grey 204)
	triangle (P2 288 18) (P2 351 360) (P2 288 360)

	fill (grey 255)
	-- todo
	-- arc (479, 300, 280, 280, PI, TWO_PI);
