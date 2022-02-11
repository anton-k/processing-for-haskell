-- original code: https://processing.org/examples/polartocartesian.html

-- PolarToCartesian by Daniel Shiffman.
--
-- Convert a polar coordinate (r,theta) to cartesian (x,y): x = rcos(theta) y = rsin(theta)
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360
center = 0.5 *^ (P2 width height)

radius = height * 0.45
thetaAcc = 0.0001

setup = do
  size (P2 width height)
  return (0, 0)

draw (theta, thetaVel) = do
  background (grey 0)
  translate center
  ellipseMode Center
  noStroke
  fill (grey 200)
  ellipse p 32
  where
    p = polarToCartesian (P2 radius theta)

update (theta, thetaVel) = return (theta1, thetaVel1)
  where
    theta1    = theta + thetaVel1
    thetaVel1 = thetaVel + thetaAcc

polarToCartesian :: P2 -> P2
polarToCartesian (P2 r theta) = r *^ P2 (cos theta) (sin theta)
