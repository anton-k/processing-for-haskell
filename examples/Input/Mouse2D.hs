-- original code: https://processing.org/examples/mouse2d.html

-- Mouse 2D.
--
-- Moving the mouse changes the position and size of each box.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (P2 width height)
  noStroke
  -- todo implement
  rectMode Center

draw () = do
  background (grey 51)
  m@(P2 mx my) <- mouse

  fill (greya 255 204)
  rect (P2 mx (height/2)) (0.5 *^ (P2 my my) + 10)

  let inverseX = width  - mx
      inverseY = height - my

  fill (greya 255 204)
  rect (P2 inverseX (height/2)) (0.5 *^ (P2 inverseY inverseY) + 10)
