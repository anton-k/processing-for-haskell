-- Storing Input.
--
-- Move the mouse across the screen to change the position of the circles.
-- The positions of the mouse are recorded into an array and played back
-- every frame. Between each frame, the newest value are added to the end
-- of each array and the oldest value is deleted.
import qualified Data.Sequence as S
import Data.Foldable

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

center = 0.5 *^ (P2 width height)
num = 60

initQueue x = S.fromList $ take (int num) $ repeat x

setup = do
  size (P2 width height)
  noStroke
  fill (greya 255 153)
  return (initQueue center)

draw ps = do
  background (grey 51)
  forM_ (zip [0 .. num] (toList ps)) $ \(i, p) -> do
    fill (greya 255 (255 - 3 * num))
    ellipse p (P2 i i)

update ps = do
  m <- mouse
  return (put m ps)

put value xs = S.drop 1 xs S.|> value
