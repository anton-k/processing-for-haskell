module Graphics.Proc.Lib.Shape.Curve(
	bezier	
) where

import Graphics.Proc.Core
import Graphics.Proc.Lib.Shape.Primitive2D

bezierPointsNum = 35

-- | Draws a Bezier curve on the screen. These curves are defined by a series 
-- of anchor and control points. The first two parameters specify the first 
-- anchor point and the last two parameters specify the other anchor point. 
-- The middle parameters specify the control points which define the shape of the curve. 
-- Bezier curves were developed by French engineer Pierre Bezier. 
--
-- processing docs: <https://processing.org/reference/bezier_.html>
bezier :: P2 -> P2 -> P2 -> P2 -> Draw
bezier p1 p2 p3 p4 = linePath (map phi ts)
    where
        ts = [0, 1 / bezierPointsNum .. 1]
        phi t = ((1 - t) ** 3) *^ p1 + (3 * t * ((1 - t) ** 2)) *^ p2 + (3 * (t ** 2) * (1 - t)) *^ p3 + (t ** 3) *^ p4
