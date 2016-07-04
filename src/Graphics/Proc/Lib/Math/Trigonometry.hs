-- | We can use all functions defined in Haskell
module Graphics.Proc.Lib.Math.Trigonometry(	
	radians, degrees, e, erad
) where

import Graphics.Proc.Core
import Graphics.Proc.Lib.Math.Calculation

-- | Converts degrees to radians.
radians :: Float -> Float
radians = remap (0, 360) (0, 2 * pi)

-- | Converts rdians to degrees.
degrees :: Float -> Float
degrees = remap (0, 2 * pi) (0, 360)

-- | Converts angle in taus to unit vector rotated by given angle.
e :: Float -> P2
e x = (cos (2 * pi * x), sin (2 * pi * x))

-- | The function e in radians.
erad :: Float -> P2
erad x = (cos x, sin x)