-- original code: https://processing.org/examples/doublerandom.html

-- Double Random by Ira Greenberg.
--
-- Using two random() calls and the point() function to create an irregular sawtooth line.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }
 
width  = 640
height = 360

totalPts = 300
steps = totalPts + 1
dx = width / steps

setup = do
  size (width, height)
  stroke (grey 255)
  randRef <- newPioRef 0
  return randRef
  -- todo implement
  -- frameRate 1

draw randRef = do
	background (grey 0)
	writePioRef randRef 0
	forM_ [0 .. steps ] $ \i -> do
		rand <- readPioRef randRef
		r1 <- random2 (-rand, rand)
		point (dx * i, height / 2 + r1)
		r2 <- random2 (-5, 5)
		modifyPioRef randRef (+ r2)
