-- original code
--
-- https://processing.org/examples/functions.html

-- Functions.

-- The drawTarget() function makes it easy to draw many distinct targets.
-- Each call to drawTarget() specifies the position,
-- size, and number of rings for each target.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (P2 width height)
  noStroke

draw ()  = do
  background (grey 51)
  drawTarget (width*0.25) (height*0.4) 200 4
  drawTarget (width*0.5)  (height*0.5) 300 10
  drawTarget (width*0.75) (height*0.3) 120 6

drawTarget xloc yloc size num = do
  forM_ [0, 1 .. num] $ \i -> do
    fill (grey (i*grayvalues))
    ellipse (P2 xloc yloc) (P2 (size - i*steps) (size - i*steps))
  where
    grayvalues = 255/num
    steps = size/num
