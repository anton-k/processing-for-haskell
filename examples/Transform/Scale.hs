-- original code: https://processing.org/examples/scale.html

-- Scale by Denis Grutze.
-- 
-- Paramenters for the scale() function are values specified as decimal 
-- percentages. For example, the method call scale(2.0) will increase 
-- the dimension of the shape by 200 percent. Objects always scale from the origin.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

dim = 80.0

setup = do    
  size (width, height)
  noStroke
  -- need to implement
  -- rectMode(CENTER);
  -- frameRate(30); 
  return 0

draw a = do 
  background (grey 102)  

  translate (width/2, height/2)  
  scale (s, s)
  fill (grey 51);
  rect (0, 0) (50, 50)

  translate (75, 0)
  fill (grey 255)
  scale (s, s)
  rect (0, 0) (50, 50)
  where 
  	s = cos a * 2
  
update a = return (a + 0.004)
