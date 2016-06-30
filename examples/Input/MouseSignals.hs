-- original code: https://processing.org/examples/mousesignals.html

-- Mouse Signals.
--
-- Move and click the mouse to generate signals. The top row is the signal 
-- from "mouseX", the middle row is the signal from "mouseY", and the 
-- bottom row is the signal from "mousePressed".
import qualified Data.Sequence as S
import Data.Foldable

import Graphics.Proc

main = runProc $ def { 
    procSetup = setup, procDraw = draw, procUpdate = update, 
    procMousePressed = mousePressed, procMouseReleased = mouseReleased }
 
width  = 640
height = 360

initQueue x = S.fromList $ take (int width) $ repeat x

setup = do
  size (width, height)
  noSmooth
  return (initQueue 0, initQueue 0, initQueue 1, False)

draw (xs, ys, ps, _) = do
  background (grey 102)
  fill (grey 255)
  noStroke
  rect (0, height/3 - 2) (width, height/3 + 2)

  stroke (grey 255)
  pointSeq $ fmap (/ 3) xs

  stroke (grey 0)
  pointSeq $ fmap (\y -> height/3+y/3) ys

  stroke (grey 255)
  lineSeq  $ fmap (\p -> 2*height/3 + p/3 + 7) ps

pointSeq xs = pointPath $ toPoints xs
lineSeq  xs = linePath  $ toPoints xs
toPoints xs = zip [1 .. width] (toList xs)

update (xs, ys, ps, isPress) = do
  (mx, my) <- mouse
  return (put mx xs, put my ys, put (if isPress then 0 else 255) ps, isPress)

put value xs = S.drop 1 xs S.|> value

mousePressed  (xs, ys, ps, _) = return (xs, ys, ps, True) 
mouseReleased (xs, ys, ps, _) = return (xs, ys, ps, False) 
