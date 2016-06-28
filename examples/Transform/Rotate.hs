-- original code: https://processing.org/examples/rotate.html

-- Rotate.
-- 
-- Rotating a square around the Z axis. To get the results you expect, 
-- send the rotate function angle parameters that are values 
-- between 0 and PI*2 (TWO_PI which is roughly 6.28). If you prefer 
-- to think about angles as degrees (0-360), you can use the radians() 
-- method to convert your values. For example: scale(radians(90)) is 
-- identical to the statement scale(PI/2).


import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

dim = 80.0

setup = do    
  size (width, height)
  noStroke
  fill (grey 255)
  -- need to implement
  -- rectMode(CENTER);  
  return (0, 0)

draw (angle, _) = do 
  background (grey 51)  

  let c = cos angle  
  translate (width/2, height/2)  
  rotate c
  rect (0, 0) (180, 180)
  where 
  	s = cos angle * 2
  
update (angle, jitter) = do
	s <- second
	jitter1 <- if (s `mod` 2 == 0) 
				then random2 (-0.1, 0.1)
				else return jitter	
	return (angle + jitter1, jitter1)
	
