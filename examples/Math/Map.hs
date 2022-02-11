-- Map.
--
-- Use the map() function to take any number and scale it to a new
-- number that is more useful for the project that you are working on.
-- For example, use the numbers from the mouse position to control
-- the size or color of a shape. In this example, the mouse’s x-coordinate
-- (numbers between 0 and 360) are scaled to new numbers to define the
-- color and size of a circle.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

center = 0.5 *^ (P2 width height)

setup = do
  size (P2 width height)
  noStroke

draw () = do
	background (grey 0)
	mx <- mouseX
	-- Scale the mouseX value from 0 to 640 to a range between 0 and 175
	let c = remap (0, width) (0, 175) mx
	-- Scale the mouseX value from 0 to 640 to a range between 40 and 300
	    d = remap (0, width) (20, 150) mx
	fill (rgb 255 c 0)
	circle d center

------------------------------------------------
-- Side note

-- Haskell already has the function map, so the Processing's function is called `remap`.
