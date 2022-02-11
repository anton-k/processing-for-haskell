module Graphics.Proc.Lib.Math.Calculation(
  remap, FloatInterval, constrain, constrain2
) where

import Graphics.Proc.Core

-- | Interval for Float value @(minValue, maxValue)@.
type FloatInterval = (Float, Float)

-- | Re-maps a number from one range to another. Originally called map in the Processing, but in Haskell this name is already taken.
--
-- processing docs: <https://processing.org/reference/map_.html>
remap :: FloatInterval -> FloatInterval -> Float -> Float
remap (a, b) (a1, b1) x =  a1 + (b1 - a1) * (x - a) / (b - a)

-- | The @constrian@ that is defined on vectors.
constrain2 :: (P2, P2) -> P2 -> P2
constrain2 ((P2 xmin ymin), (P2 xmax ymax)) (P2 x y) = P2 (constrain (xmin, xmax) x) (constrain (ymin, ymax) y)

-- | Constrains a value to not exceed a maximum and minimum value.
--
-- processing docs: <https://processing.org/reference/constrain_.html>
constrain :: (Float, Float) -> Float -> Float
constrain (xmin, xmax) x = min xmax (max xmin x)
