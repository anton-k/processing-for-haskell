-- original code: https://processing.org/examples/mouse1d.html

-- Mouse 1D.
--
-- Move the mouse left and right to shift the balance. The "mouseX" variable
-- is used to control both the size and color of the rectangles.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (P2 width height)
  noStroke
  -- todo implement
  -- colorMode(RGB, height, height, height);
  rectMode Center

grey' = grey . remap (0, height) (0, 255)

draw () = do
  background (grey 0)
  mx <- mouseX
  let r1 = remap (0, width) (0, height) mx
      r2 = height - r1

  fill (grey' r1)
  rect (P2 (width/2 + r1/2) (height/2)) (P2 r1 r1)

  fill (grey' r2)
  rect (P2 (width/2 - r2/2) (height/2)) (P2 r2 r2)
