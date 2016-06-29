module Graphics.Proc.Lib.Math.Calculation(	
	remap, FloatInterval
) where

import Graphics.Proc.Core

type FloatInterval = (Float, Float)

remap :: FloatInterval -> FloatInterval -> Float -> Float
remap (a, b) (a1, b1) x =  a1 + (b1 - a1) * (x - a) / (b - a)	