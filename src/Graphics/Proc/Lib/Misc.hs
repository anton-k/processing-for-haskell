module Graphics.Proc.Lib.Misc(
    onCircle, onLine, uon
) where

import Graphics.Proc.Core

onCircle :: Float -> P2 -> Float -> P2
onCircle rad center x = center + rad *^ (cos (2 * pi * x), sin (2 * pi * x)) 

onLine :: P2 -> P2 -> Float -> P2
onLine p1 p2 x = p1 + x *^ (p2 - p1)

uon :: (Float, Float) -> Float -> Float
uon (a, b) x = a + (b - a) * x