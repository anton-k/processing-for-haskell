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
  printTime

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

printTime = do
  h <- hour
  m <- minute
  s <- second
  println (show h ++ ":" ++ show m ++ ":" ++ show s)


{-
void draw() {
  background(0);
  
  // Draw the clock background
  fill(80);
  noStroke();
  ellipse(cx, cy, clockDiameter, clockDiameter);
  
  // Angles for sin() and cos() start at 3 o'clock;
  // subtract HALF_PI to make them start at the top
  float s = map(second(), 0, 60, 0, TWO_PI) - HALF_PI;
  float m = map(minute() + norm(second(), 0, 60), 0, 60, 0, TWO_PI) - HALF_PI; 
  float h = map(hour() + norm(minute(), 0, 60), 0, 24, 0, TWO_PI * 2) - HALF_PI;
  
  // Draw the hands of the clock
  stroke(255);
  strokeWeight(1);
  line(cx, cy, cx + cos(s) * secondsRadius, cy + sin(s) * secondsRadius);
  strokeWeight(2);
  line(cx, cy, cx + cos(m) * minutesRadius, cy + sin(m) * minutesRadius);
  strokeWeight(4);
  line(cx, cy, cx + cos(h) * hoursRadius, cy + sin(h) * hoursRadius);
  
  // Draw the minute ticks
  strokeWeight(2);
  beginShape(POINTS);
  for (int a = 0; a < 360; a+=6) {
    float angle = radians(a);
    float x = cx + cos(angle) * secondsRadius;
    float y = cy + sin(angle) * secondsRadius;
    vertex(x, y);
  }
  endShape();
}
-}