module Graphics.Proc.Lib3.Shape.Primitive2D(
  triangle, quad,
  line, linePath,
  point, pointPath, polygon,
) where

import Graphics.Proc.Core
import Graphics.Rendering.OpenGL hiding (get, rect)
import qualified Graphics.Proc.Core.Vector.Primitive2D as Core

-- | A triangle is a plane created by connecting three points.
--
-- processing docs: <https://processing.org/reference/triangle_.html>
triangle :: P3 -> P3 -> P3 -> Draw
triangle = Core.triangle

-- | Draws a rectangle to the screen. A rectangle is a four-sided shape
-- with every angle at ninety degrees. By default, the first two parameters
-- set the location of the upper-left corner, the third sets the width,
-- and the fourth sets the height. The way these parameters are interpreted,
-- however, may be changed with the rectMode() function.
--

-- | A quad is a quadrilateral, a four sided polygon. It is similar to a rectangle,
-- but the angles between its edges are not constrained to ninety degrees. The first
-- pair of parameters (x1,y1) sets the first vertex and the subsequent pairs should
-- proceed clockwise or counter-clockwise around the defined shape.
--
-- processing docs: <https://processing.org/reference/quad_.html>
quad :: P3 -> P3 -> P3 -> P3 -> Draw
quad = Core.quad

-- | Draws a polygon.
polygon :: [P3] -> Draw
polygon = Core.polygon

-- | Draws a point, a coordinate in space at the dimension of one pixel.
--
-- processing docs: <https://processing.org/reference/point_.html>
point :: P3 -> Draw
point = Core.point

-- | Draws a sequence of points.
pointPath :: [P3] -> Draw
pointPath = Core.pointPath

setStrokeColor :: Pio ()
setStrokeColor = setCol . maybe black id =<< getStroke

black = Col 0 0 0 1

-- | Draws a line (a direct path between two points) to the screen.
--
-- processing docs: <https://processing.org/reference/line_.html>
line :: P3 -> P3 -> Draw
line = Core.line

-- | Draws a line-path (sequence of line segments).
linePath :: [P3] -> Draw
linePath = Core.linePath

---------------------------------------------------

setCol :: Col -> Draw
setCol col = liftIO $ currentColor $= glCol col
