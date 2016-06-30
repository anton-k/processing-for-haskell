-- original code: https://processing.org/examples/arctangent.html

-- Arctangent.
-- 
-- Move the mouse to change the direction of the eyes. The atan2() function computes the angle from each eye to the cursor.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }
 
data Eye = Eye 
	{ eyePos :: P2	
	, eyeSize :: Float
	, eyeAngle :: Float
	}

width  = 640
height = 360

setup = do
  size (width, height)
  noStroke
  return [e1, e2, e3]
  where
  	e1 = Eye (250, 16)  120 0
	e2 = Eye (164, 185) 80  0
	e3 = Eye (420, 230) 220 0
	
draw eyes = do
	background (grey 102)
	mapM_ drawEye eyes

update eyes = do
	m <- mouse
	return $ fmap (updateEye m) eyes

drawEye x = local $ do
	translate (eyePos x)
	fill (grey 255)
	ellipse 0 sz
	rotate (eyeAngle x)
	fill (rgb 153 204 0)
	ellipse (eyeSize x / 4, 0) (0.5 *^ sz)	
	where
		sz = (eyeSize x, eyeSize x)

updateEye m eye = eye { eyeAngle = remap (0, 2 * pi) (0, 1) (atan2 y x) }
	where 
		(x, y) = m - eyePos eye
