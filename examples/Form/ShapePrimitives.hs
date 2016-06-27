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
	size (width, height);
	noStroke

draw () = do
	background (grey 0)

	fill (grey 204)
	triangle (18, 18) (18, 360) (81, 360)

	fill (grey 102)
	rect (81, 81) (63, 63)

	fill (grey 204);
	quad (189, 18) (216, 18) (216, 360) (144, 360)

	fill (grey 255)
	ellipse (252, 144) (72, 72)

	fill (grey 204)
	triangle (288, 18) (351, 360) (288, 360)

	fill (grey 255)
	-- todo
	-- arc (479, 300, 280, 280, PI, TWO_PI);
