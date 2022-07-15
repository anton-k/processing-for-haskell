module Graphics.Proc.Core.Vector.Primitive2D(
  triangle, quad,
  line, linePath,
  point, pointPath, polygon,
) where

import Graphics.Proc.Core
import Graphics.Rendering.OpenGL hiding (get, rect)

-- | A triangle is a plane created by connecting three points.
--
-- processing docs: <https://processing.org/reference/triangle_.html>
triangle :: IsPoint p => p -> p -> p -> Draw
triangle p1 p2 p3 = drawProcP3 Triangles LineLoop [p1, p2, p3]

-- | A quad is a quadrilateral, a four sided polygon. It is similar to a rectangle,
-- but the angles between its edges are not constrained to ninety degrees. The first
-- pair of parameters (x1,y1) sets the first vertex and the subsequent pairs should
-- proceed clockwise or counter-clockwise around the defined shape.
--
-- processing docs: <https://processing.org/reference/quad_.html>
quad :: IsPoint p => p -> p -> p -> p -> Draw
quad p1 p2 p3 p4 = drawProcP3 Polygon LineLoop [p1, p2, p3, p4]

-- | Draws a polygon.
polygon :: IsPoint p => [p] -> Draw
polygon ps = drawProcP3 Polygon LineLoop ps

-- | Draws a point, a coordinate in space at the dimension of one pixel.
--
-- processing docs: <https://processing.org/reference/point_.html>
point :: IsPoint p => p -> Draw
point p = do
  setStrokeColor
  drawP3 Points [p]

-- | Draws a sequence of points.
pointPath :: IsPoint p => [p] -> Draw
pointPath ps = do
  setStrokeColor
  drawP3 Points ps

setStrokeColor :: Pio ()
setStrokeColor = setCol . maybe black id =<< getStroke

black = Col 0 0 0 1

-- | Draws a line (a direct path between two points) to the screen.
--
-- processing docs: <https://processing.org/reference/line_.html>
line :: IsPoint p => p -> p -> Draw
line p1 p2 = do
  setStrokeColor
  drawP3 Lines [p1, p2]

-- | Draws a line-path (sequence of line segments).
linePath :: IsPoint p => [p] -> Draw
linePath ps = do
  setStrokeColor
  drawP3 LineStrip ps

---------------------------------------------------

drawP3 :: IsPoint p => PrimitiveMode -> [p] -> Pio ()
drawP3 primType ps = do
  liftIO $ renderPrimitive primType $ mapM_ v3 ps

drawProcP3 :: IsPoint p => PrimitiveMode -> PrimitiveMode -> [p] -> Pio ()
drawProcP3 onFill onStroke ps = do
    go onFill   =<< getFill
    go onStroke =<< getStroke
    where
        go shapeType mcol = case mcol of
          Just col -> do
            setCol col
            drawP3 shapeType ps
          Nothing -> return ()

setCol :: Col -> Draw
setCol col = liftIO $ currentColor $= glCol col
