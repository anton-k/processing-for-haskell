-- original code: https://processing.org/examples/redraw.html

-- Redraw. 
-- 
-- The redraw() function makes draw() execute once. In this example, draw() 
-- is executed once every time the mouse is clicked.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update, procMousePressed = mousePressed }

width  = 640
height = 360

-- The statements in the setup() function 
-- execute once when the program begins
setup = do
  size (width, height)
  stroke (grey 255)  
  noLoop
  return (height / 2)

-- The statements in draw() are executed until the 
-- program is stopped. Each statement is executed in 
-- sequence and after the last line is read, the first 
-- line is executed again.
draw y = do  
  background (grey 0)       -- Clear the screen with a black background
  line (0, y) (width, y)
 
update y
    | y < 0     = return height
    | otherwise = return (y - 4)

mousePressed y = do
    redraw
    return y

------------------------------------------------
-- Notice that in haskell we use two separate functions to
-- draw the state and to update it.