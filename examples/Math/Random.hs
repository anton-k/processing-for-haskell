-- original code: https://processing.org/examples/random.html

-- Random.
--
-- Random numbers create the basis of this image. Each time the program is loaded the result is different.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (P2 width height)
  strokeWeight 20
  frameRate 2

draw () = do
	forM_ [0 .. width] $ \i -> do
		r <- random 255
		stroke (grey r)
		line (P2 i 0) (P2 i height)
