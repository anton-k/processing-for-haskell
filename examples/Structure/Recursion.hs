-- original code
--
-- https://processing.org/examples/recursion.html

-- Recursion.
--
-- A demonstration of recursion, which means functions call themselves. 
-- Notice how the drawCircle() function calls itself at the end of its block. 
-- It continues to do this until the variable "level" is equal to 1.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (width, height)  
  noStroke  
 
draw () = do
  background (grey 255)
  drawCircle (width/2) 280 6

drawCircle x radius level = do
  let tt = 126 * level/4.0
  fill (grey tt)
  ellipse (x, height/2) (radius*2, radius*2)
  if (level > 1)
    then do
      let newLevel = level - 1
      drawCircle (x - radius/2) (radius/2) newLevel
      drawCircle (x + radius/2) (radius/2) newLevel
    else do
      return ()
