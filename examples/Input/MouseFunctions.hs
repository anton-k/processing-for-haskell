-- original code: https://processing.org/examples/mousefunctions.html

-- Mouse Functions.
-- 
-- Click on the box and drag it across the screen.
import Data.Maybe
import Graphics.Proc hiding (scale)

main = runProc $ def 
  { procSetup = setup, procDraw = draw, procUpdate = update
  , procMousePressed = mousePressed, procMouseReleased = mouseReleased }
 
width  = 640
height = 360

boxSize = 75
center = 0.5 *^ (width, height)

setup = do
  size (width, height)  
  rectMode Radius
  noStroke
  return (center, mlockPoint)
  where mlockPoint = Nothing

getCol lockPoint
  | isLocked  = grey 255
  | otherwise = grey 153
  where isLocked = isJust lockPoint

draw (pos, lockPoint) = do
  background (grey 0)
  stroke (grey 255)
  fill (getCol lockPoint)
  rect pos (boxSize, boxSize)

update st@(pos, mlockPoint) = case mlockPoint of
  Just lockPoint -> do
    m <- mouse
    let pos1 = m + lockPoint
    return (pos1, mlockPoint)
  Nothing -> return st

mousePressed  = whenWhithin $ \m (pos, _) -> (pos, Just (pos - m))
mouseReleased = whenWhithin $ \_ (pos, _) -> (pos, Nothing)
 
whenWhithin f (pos, value) = do
  m <- mouse
  return $ if (withinRect m pos)
    then f m (pos, value)
    else (pos, value)

withinRect (mx, my) (px, py) = 
  mx >= (px - boxSize) && mx < (px + boxSize) &&
  my >= (py - boxSize) && my < (py + boxSize) 
