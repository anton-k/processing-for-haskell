-- original code: https://processing.org/examples/translate.html

-- Translate.
-- 
-- The translate() function allows objects to be moved to any 
-- location within the window. The first parameter sets the 
-- x-axis offset and the second parameter sets the y-axis offset.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

dim = 80.0

setup = do    
  size (width, height)
  noStroke
  return 0

draw x = do 
  background (grey 102)  
  
  translate (x, height/2 - dim/2)
  fill (grey 255)
  rect (-dim/2, -dim/2) (dim, dim)
  
  -- Transforms accumulate. Notice how this rect moves 
  -- twice as fast as the other, but it has the same 
  -- parameter for the x-axis value
  translate (x, dim)
  fill (grey 0)
  rect (-dim/2, -dim/2) (dim, dim)

update x = return (if (x > width + dim) then (-dim) else x + 1)
