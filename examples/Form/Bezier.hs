-- original code: https://processing.org/examples/bezier.html
--

-- Bezier. 
--
-- The first two parameters for the bezier() function specify the 
-- first point in the curve and the last two parameters specify 
-- the last point. The middle parameters set the control points that 
-- define the shape of the curve.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do  
  size (width, height)
  stroke (grey 255)
  noFill

draw () = do
  background (grey 0)
  mx <- mouseX
  forM_ [0, 20 .. 200] $ \i -> do
    bezier (mx - i/2, 40 + i) (410, 20) (440, 300) (240 - (i/16), 300 + (i/8))
