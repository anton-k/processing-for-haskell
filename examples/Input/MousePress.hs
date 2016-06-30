-- original code: https://processing.org/examples/mousepress.html

-- Mouse Press.
-- 
-- Move the mouse to position the shape. Press the mouse button to invert the color.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procMousePressed = mousePressed, procMouseReleased = mouseReleased }
 
width  = 640
height = 360

setup = do
  size (width, height)
  noSmooth
  fill (grey 126)
  background (grey 102)
  return 0

draw col = do
  stroke (grey col)
  (mX, mY) <- mouse
  line (mX-66, mY) (mX+66, mY)
  line (mX, mY-66) (mX, mY+66)

mousePressed  _ = return 255
mouseReleased _ = return 0