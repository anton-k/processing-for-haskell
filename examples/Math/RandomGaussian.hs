-- original code: https://processing.org/examples/randomgaussian.html

-- Random Gaussian.
--
-- This sketch draws ellipses with x and y locations tied to a gaussian distribution of random numbers.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

totalPts = 300
steps = totalPts + 1
dx = width / steps

setup = do
  size (P2 width height)
  background (grey 0)

draw () = do
	drawCircle =<< getRnd

drawCircle x = do
	noStroke
	fill (greya 255 10)
	ellipse (P2 x (height/2)) (P2 32 32)

getRnd = do
	value <- randomGaussian
	return (( value * sd ) + mean)

sd = 60                  -- Define a standard deviation
mean = width/2           -- Define a mean value (middle of the screen along the x-axis)
