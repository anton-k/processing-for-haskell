-- original code: https://processing.org/examples/arm.html

-- Arm.
-- 
-- The angle of each segment is controlled with the mouseX and mouseY position. 
-- The transformations applied to the first segment are also applied to the 
-- second segment because they are inside the same pushMatrix() and popMatrix() group.
--

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

base = (width * 0.3, height * 0.5)
segLength = 100

setup = do    
  size (width, height)
  strokeWeight 30
  stroke (greya 255 160)
  return (0, 0)

draw (angle1, angle2) = do
    background (grey 0)
    local $ do
        segment base angle1
        segment (segLength, 0) angle2

update _ = do
    -- relMouse produces mouse pointer coordinates normalized with sizes of the window
    (mx, my) <- relMouse 
    let angle1 = -0.5 * (mx - 0.5)
        angle2 =  0.5 * (my - 0.5)
    return (angle1, angle2)

segment p a = do
    translate p
    rotate a
    line (0, 0) (segLength, 0)

---------------------------------------------------
-- Side note
--
-- The rotate in processing varies in the interval [0, 2*pi]
-- But in the haskell lib the angle of rotation is measured in TAUs
-- It's a ratio of full rotation. So the interval is [0, 1].
