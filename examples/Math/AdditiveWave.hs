-- originaal code: https://processing.org/examples/additivewave.html

-- Additive Wave by Daniel Shiffman.
-- 
-- Create a more complex wave by adding two waves together.
import Data.List

import Graphics.Proc 

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

xspacing = 8   -- How far apart should each horizontal location be spaced
w = width + 16 -- Width of entire wave
maxwaves = 4   -- total # of waves to add together
npoints = int $ w / xspacing

theta = 0.0

--  amplitudes  -- Height of wave
--  dxs         -- Value for incrementing X, to be calculated as a function of period and xspacing

setup = do
  size (width, height)
  --frameRate 30
  -- colorMode(RGB, 255, 255, 255, 100)
  (amplitudes, periods) <- fmap unzip $ forM [0 .. maxwaves] $ \i -> do
    amplitude <- random2 (10, 30)
    period    <- random2 (100, 300)
    return (amplitude, period)
  let dxs = fmap (\period -> (2 * pi / period) * xspacing) periods
  return (theta, amplitudes, dxs)
  where theta = 0

draw (theta, amplitudes, dxs) = do 
  background (grey 0)
  renderWave (calcWave theta amplitudes dxs)

-- Increment theta (try different values for 'angular velocity' here
update (theta, amplitudes, dxs) =
  return (theta + 0.012, amplitudes, dxs)

calcWave theta amplitudes dxs = 
  fmap sum $ transpose $ zipWith3 (wave1 theta) amplitudes dxs [0 .. maxwaves]

wave1 theta amplitude dx n = take npoints $ fmap (\x -> dx + amplitude * func (x + theta)) [0, dx .. ]
  where 
    func 
      | n `mod` 2 == 0 = sin
      | otherwise      = cos


renderWave ys = do
  -- A simple way to draw the wave with an ellipse at each location
  noStroke
  fill (greya 255 78)
  ellipseMode Center
  forM_ (zip [0 .. npoints] ys) $ \(x, y) -> do
    ellipse (float x * xspacing, height/2+y) 16
