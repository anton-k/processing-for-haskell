-- original code: https://processing.org/examples/interpolate.html

-- Linear Interpolation.
-- 
-- Move the mouse across the screen and the symbol will follow. 
-- Between drawing each frame of the animation, the ellipse moves 
-- part of the distance (0.05) from its current position toward 
-- the cursor using the lerp() function * This is the same as 
-- the Easing under input only with lerp() instead.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }
 
width  = 640
height = 360
center = 0.5 *^ (width, height)

setup = do
  size (width, height)
  noStroke
  return center

draw pos = do
  background (grey 51)
  fill (grey 255)
  ellipse pos 66  

update pos = do
  m <- mouse
  return (lerp pos m 0.0065)

-----------------------------------------
-- Side note
--
-- In Haskell `lerp` is defined not only for floats but also for vectors.
