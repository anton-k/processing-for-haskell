-- original code: https://processing.org/examples/mousepress.html

-- Mouse Press.
--
-- Move the mouse to position the shape. Press the mouse button to invert the color.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procMousePressed = mousePressed, procMouseReleased = mouseReleased }

width  = 640
height = 360

setup = do
  size (P2 width height)
  noSmooth
  fill (grey 126)
  background (grey 102)
  return 0

draw col = do
  stroke (grey col)
  (P2 mX mY) <- mouse
  line (P2 (mX-66) mY) (P2 (mX+66) mY)
  line (P2 mX (mY-66)) (P2 mX (mY+66))

mousePressed  _ = return 255
mouseReleased _ = return 0
