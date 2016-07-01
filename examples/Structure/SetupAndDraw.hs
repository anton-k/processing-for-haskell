-- original code:
--
-- https://processing.org/examples/setupdraw.html
--
--  The code inside the draw() function runs continuously from top to bottom until the program is stopped.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

-- The statements in the setup() function 
-- execute once when the program begins
setup = do
  size (width, height)
  stroke (grey 255)
  frameRate 30
  return 100

-- The statements in draw() are executed until the 
-- program is stopped. Each statement is executed in 
-- sequence and after the last line is read, the first 
-- line is executed again.
draw y = do  
  background (grey 0)   	-- Clear the screen with a black background
  line (0, y) (width, y)
 
update y
	| y < 0     = return height
	| otherwise = return (y - 1)

------------------------------------------------
-- Notice that in haskell we use two separate functions to
-- draw the state and to update it.