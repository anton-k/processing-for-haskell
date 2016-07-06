-- original code: https://processing.org/examples/sinewave.html
--
-- Sine Wave by Daniel Shiffman.
-- 
-- Render a simple sine wave.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

xspacing = 16;    -- How far apart should each horizontal location be spaced
w = width + 16    -- Width of entire wave
amplitude = 75.0  -- Height of wave
period = 500.0    -- How many pixels before the wave repeats
dx = (2 * pi / period) * xspacing   -- Value for incrementing X, a function of period and xspacing
npoints = int $ w / xspacing

setup = do
  size (width, height)
  return theta
  where theta = 0.0       -- Start angle at 0

draw theta = do
  background (grey 0)
  renderWave (calcWave theta)  

update theta = return (theta + 0.03)

calcWave theta = take npoints $ fmap (\x -> amplitude * sin (x + theta)) [0, dx ..]

renderWave ys = do
  noStroke
  fill (grey 255)
  forM_ (zip [0 .. npoints] ys) $ \(x, y) -> do
    ellipse (float x * xspacing, height/2 + y) 16
