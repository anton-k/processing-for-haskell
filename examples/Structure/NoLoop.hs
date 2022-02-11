-- original code
--
-- https://processing.org/examples/noloop.html

-- No Loop.
--
-- The noLoop() function causes draw() to only execute once.
-- Without calling noLoop(), the code inside draw() is run continually.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

-- The statements in the setup() function
-- execute once when the program begins
setup = do
  size (P2 width height)
  stroke (grey 255)
  noLoop
  return (height / 2)

-- The statements in draw() are executed until the
-- program is stopped. Each statement is executed in
-- sequence and after the last line is read, the first
-- line is executed again.
draw y = do
  background (grey 0)       -- Clear the screen with a black background
  line (P2 0 y) (P2 width y)

update y
    | y < 0     = return height
    | otherwise = return (y - 1)

------------------------------------------------
-- Notice that in haskell we use two separate functions to
-- draw the state and to update it.
