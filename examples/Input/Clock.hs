-- original code: https://processing.org/examples/clock.html

-- Clock.
-- 
-- The current time can be read with the second(), minute(), and hour() 
-- functions. In this example, sin() and cos() values are used to set the 
-- position of the hands.

import Graphics.Proc hiding (scale)

main = runProc $ def { procSetup = setup, procDraw = draw }
 
width  = 640
height = 360

setup = do
  size (width, height)
  noStroke

radius = min width height / 2
secondsRadius = radius * 0.72
minutesRadius = radius * 0.60
hoursRadius = radius * 0.50
clockDiameter = radius * 1.8

center = 0.5 *^ (width, height)

draw () = do
  background (grey 0)
  drawClockBackground
  drawHands
  drawTicks

drawClockBackground = do 
  fill (grey 80)
  noStroke
  ellipse center (clockDiameter, clockDiameter)

drawTicks = do
  fill (grey 255)
  mapM_ (circle 2) $ fmap (onCircle secondsRadius center) [0, 1/60 .. 1]

drawHands = do
  drawHour
  drawSecond
  drawMinute  

drawHour   = drawHand hoursRadius hour 12 5
drawMinute = drawHand minutesRadius minute 60 3
drawSecond = drawHand secondsRadius second 60 1

drawHand rad getter maxVal weight = do
  p <- getVec
  stroke (grey 255)
  strokeWeight weight
  line center p
  where
    getVec = do
      value <- getter
      return $ onCircle rad center $ (remap (0, maxVal) (0, 1) (float value)) - 0.25
