module Graphics.Proc.Lib.Misc(
    onCircle, onLine, uon
) where

import Graphics.Proc.Core

-- | Maps values from interval (0, 1) to the points on the circle.
--
-- > onCircle radius center value
onCircle :: Float -> P2 -> Float -> P2
onCircle rad center x = center + rad *^ (cos (2 * pi * x), sin (2 * pi * x)) 

-- | Maps values from interval (0, 1) to the points on the line segment.
-- 
-- > onLine point1 point2 value
onLine :: P2 -> P2 -> Float -> P2
onLine p1 p2 x = p1 + x *^ (p2 - p1)

-- | Rescales the unipolar scale (0, 1) to the given range.
uon :: (Float, Float) -> Float -> Float
uon (a, b) x = a + (b - a) * x