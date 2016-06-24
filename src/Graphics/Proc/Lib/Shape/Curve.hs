module Graphics.Proc.Lib.Shape.Curve(
	bezier	
) where

import Graphics.Proc.Core
import Graphics.Proc.Lib.Shape.Primitive2D

bezierPointsNum = 35

bezier :: P2 -> P2 -> P2 -> P2 -> Draw
bezier p1 p2 p3 p4 = linePath (map phi ts)
    where
        ts = [0, 1 / bezierPointsNum .. 1]
        phi t = ((1 - t) ** 3) *^ p1 + (3 * t * ((1 - t) ** 2)) *^ p2 + (3 * (t ** 2) * (1 - t)) *^ p3 + (t ** 3) *^ p4

