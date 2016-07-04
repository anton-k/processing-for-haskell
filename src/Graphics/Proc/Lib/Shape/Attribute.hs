module Graphics.Proc.Lib.Shape.Attribute(
	EllipseMode(..), ellipseMode, rectMode,
	strokeWeight
) where

import Control.Monad.Trans.State.Strict
import Graphics.Rendering.OpenGL

import Graphics.Proc.Core

-- | Modifies the location from which ellipses are drawn by changing the way in which parameters given to ellipse() are intepreted.
-- 
-- The default mode is @ellipseMode Center@, which interprets the first two parameters of ellipse() as the shape's center point, while the third and fourth parameters are its width and height.
-- 
-- @ellipseMode Radius@ also uses the first two parameters of ellipse() as the shape's center point, but uses the third and fourth parameters to specify half of the shapes's width and height.
-- 
-- @ellipseMode Corner@ interprets the first two parameters of ellipse() as the upper-left corner of the shape, while the third and fourth parameters are its width and height.
-- 
-- @ellipseMode Corners@ interprets the first two parameters of ellipse() as the location of one corner of the ellipse's bounding box, and the third and fourth parameters as the location of the opposite corner.
ellipseMode :: EllipseMode -> Draw
ellipseMode = putEllipseMode

-- | Modifies the location from which rectangles are drawn by changing the way in which parameters given to rect() are intepreted.
-- 
-- The default mode is @rectMode Corner@, which interprets the first two parameters of rect() as the upper-left corner of the shape, while the third and fourth parameters are its width and height.
-- 
-- @rectMode Corners@ interprets the first two parameters of rect() as the location of one corner, and the third and fourth parameters as the location of the opposite corner.
-- 
-- @rectMode Center@ interprets the first two parameters of rect() as the shape's center point, while the third and fourth parameters are its width and height.
--
-- @rectMode Radius@ also uses the first two parameters of rect() as the shape's center point, but uses the third and fourth parameters to specify half of the shapes's width and height.
--
-- processing docs: <https://processing.org/reference/rectMode_.html>
rectMode :: RectMode -> Draw
rectMode = putRectMode

-- | Sets the width of the stroke used for lines, points, and the border around shapes. All widths are set in units of pixels. 
--
-- processing docs: <https://processing.org/reference/strokeWelight_.html>
strokeWeight :: Float -> Draw
strokeWeight x = liftIO $ lineWidth $= x


