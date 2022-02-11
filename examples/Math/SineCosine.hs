-- original code: https://processing.org/examples/sinecosine.html

-- Sine Cosine.
--
-- Linear movement with sin() and cos(). Numbers between 0 and PI*2 (TWO_PI which angles roughly 6.28)
-- are put into these functions and numbers between -1 and 1 are returned. These values
-- are then scaled to produce larger movements.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

center = 0.5 *^ (P2 width height)

setup = do
  size (P2 width height)
  noStroke
  rectMode Center
  return (P2 0 0)

scalar = 70

draw (P2 angle1 angle2) = do
  background (grey 0)
  fill (grey 255)
  rect (P2 (width*0.5) (height*0.5)) (P2 140 140)

  fill (rgb 0 102 153)
  ellipse (P2 x1 (height*0.5 - 120)) (P2 scalar scalar)
  ellipse (P2 x2 (height*0.5 + 120)) (P2 scalar scalar)

  fill (rgb 255 204 0)
  ellipse (P2 (width*0.5 - 120) y1) (P2 scalar scalar)
  ellipse (P2 (width*0.5 + 120) y2) (P2 scalar scalar)
  where
    ang1 = radians angle1
    ang2 = radians angle2

    er x = P2 (cos x) (sin x)
    (P2 x1 y1) = center + scalar *^ (er ang1)
    (P2 x2 y2) = center + scalar *^ (er ang2)

update :: P2 -> Pio P2
update angles = return $ angles + (P2 2 3)
