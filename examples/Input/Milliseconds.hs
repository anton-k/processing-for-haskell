-- Milliseconds.
-- 
-- A millisecond is 1/1000 of a second. Processing keeps track 
-- of the number of milliseconds a program has run. By modifying 
-- this number with the modulo(%) operator, different patterns 
-- in time are created.

import Graphics.Proc hiding (scale)

main = runProc $ def { procSetup = setup, procDraw = draw }
 
width  = 640
height = 360

scale = width/20

setup = do
  size (width, height)
  noStroke
  
draw () = do
	m <- millis
	forM_ [0 .. scale] $ \i -> do
		fill (grey $ remap (0, (i + 1) * scale * 10) (0, 255) (float $ m `mod` (int $ (i + 1) * scale * 10)))
		rect (i * scale, 0) (scale, height)
