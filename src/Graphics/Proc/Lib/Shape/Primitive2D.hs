module Graphics.Proc.Lib.Shape.Primitive2D(	
	triangle, rect, quad, ellipse, 
	circle, line, linePath,
	point, pointPath, polygon,
) where

import Control.Monad.Trans.State.Strict
import Graphics.Proc.Core
import Graphics.Rendering.OpenGL hiding (get, rect)

-- | A triangle is a plane created by connecting three points. 
--
-- processing docs: <https://processing.org/reference/triangle_.html>
triangle :: P2 -> P2 -> P2 -> Draw
triangle p1 p2 p3 = drawProcP2 (Triangles, LineLoop) [p1, p2, p3]    

-- | Draws a rectangle to the screen. A rectangle is a four-sided shape 
-- with every angle at ninety degrees. By default, the first two parameters 
-- set the location of the upper-left corner, the third sets the width, 
-- and the fourth sets the height. The way these parameters are interpreted, 
-- however, may be changed with the rectMode() function.
--
-- processing docs: <https://processing.org/reference/rect_.html>
rect :: P2 -> P2 -> Draw
rect a b = uncurry cornerRect =<< fmap (\mode -> modeRectPoints mode a b) getRectMode

cornerRect :: P2 -> P2 -> Draw
cornerRect (x, y) (w, h) = drawProcP2 (Polygon, LineLoop) [(x, y), (x, y + h), (x + w, y + h), (x + w, y)]

modeRectPoints mode (a, b) (c, d) = case mode of
  Corner  -> ((a, b), (c, d))
  Corners -> ((a, b), (c - a, d - b))
  Radius  -> 
      let rx = c
          ry = d
          cx = a
          cy = b
      in ((cx - rx, cy - ry), (2 * rx, 2 * ry))
  Center ->
      let dx = c
          dy = d
          rx = dx / 2
          ry = dy / 2
          cx = a
          cy = b
      in ((cx - rx, cy - ry), (dx, dy))

-- | A quad is a quadrilateral, a four sided polygon. It is similar to a rectangle,
-- but the angles between its edges are not constrained to ninety degrees. The first 
-- pair of parameters (x1,y1) sets the first vertex and the subsequent pairs should 
-- proceed clockwise or counter-clockwise around the defined shape. 
-- 
-- processing docs: <https://processing.org/reference/quad_.html>
quad :: P2 -> P2 -> P2 -> P2 -> Draw
quad p1 p2 p3 p4 = drawProcP2 (Polygon, LineLoop) [p1, p2, p3, p4]

-- | Draws a polygon.
polygon :: [P2] -> Draw
polygon ps = drawProcP2 (Polygon, LineLoop) ps

-- | Draws a point, a coordinate in space at the dimension of one pixel. 
--
-- processing docs: <https://processing.org/reference/point_.html>
point :: P2 -> Draw
point p = do
  setStrokeColor
  drawP2 Points [p]

-- | Draws a sequence of points.
pointPath :: [P2] -> Draw
pointPath ps = do
  setStrokeColor
  drawP2 Points ps

setStrokeColor :: Pio ()
setStrokeColor = setCol . maybe black id =<< getStroke

black = Col 0 0 0 1  

-- | Draws a line (a direct path between two points) to the screen.
--
-- processing docs: <https://processing.org/reference/line_.html>
line :: P2 -> P2 -> Draw
line p1 p2 = do
  setStrokeColor
  drawP2 Lines [p1, p2]

-- | Draws a line-path (sequence of line segments).
linePath :: [P2] -> Draw
linePath ps = do
  setStrokeColor
  drawP2 LineStrip ps

-- | Draws an ellipse (oval) to the screen. An ellipse with equal 
-- width and height is a circle. By default, the first two parameters 
-- set the location, and the third and fourth parameters set the shape's 
-- width and height. The origin may be changed with the @ellipseMode()@ function. 
-- 
-- processing docs: <https://processing.org/reference/ellipse_.html>
ellipse :: P2 -> P2 -> Draw
ellipse center rad = do
  mode <- getEllipseMode
  drawProcP2 (Polygon, LineLoop) (modeEllipsePoints mode 150 rad center) 

-- | Draws a circle with a given radius and center.
--
-- > circle radius center
circle :: Float -> P2 -> Draw
circle rad center = drawProcP2 (Polygon, LineLoop) (modeEllipsePoints Radius 150 (rad, rad) center) 

modeEllipsePoints mode number (a, b) (c, d) = (uncurry $ ellipsePoints number) $ case mode of
  Center  -> 
    let width = a
        height = b
        cx = c
        cy = d
    in ((width / 2, height / 2), (cx, cy))
  Radius  ->
    let radx = a
        rady = b
        cx = c
        cy = d
    in ((radx, rady), (cx, cy))
  Corner  -> 
    let width = a
        height = b
        px = c
        py = d
        rx = width / 2
        ry = width / 2
    in ((rx, ry), (px + rx, py + ry))
  Corners ->
    let p1x = a
        p1y = b
        p2x = c
        p2y = d
    in ((abs (p1x - p2x) / 2, abs (p1y - p2y) / 2), ((p1x + p2x) / 2, (p1y + p2y) / 2))

ellipsePoints number (radx, rady) (cx, cy) = 
    [ let alpha = twoPi * i /number
      in (cx + radx * (cos (alpha)), cy + rady * (sin (alpha)))
    | i <- [1,2..number]]
    where
        twoPi = 2*pi


---------------------------------------------------

drawP2 primType ps = do   
  liftIO $ renderPrimitive primType $ mapM_ v2 ps

drawProcP2 (onFill, onStroke) ps = do    
    go onFill   =<< getFill    
    go onStroke =<< getStroke
    where        
        go shapeType mcol = case mcol of
          Just col -> do            
            setCol col
            drawP2 shapeType ps
          Nothing -> return ()

setCol :: Col -> Draw
setCol col = liftIO $ currentColor $= glCol col
