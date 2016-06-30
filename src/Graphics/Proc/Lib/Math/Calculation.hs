module Graphics.Proc.Lib.Math.Calculation(	
	remap, FloatInterval, constrain, constrain2
) where

import Graphics.Proc.Core

type FloatInterval = (Float, Float)

remap :: FloatInterval -> FloatInterval -> Float -> Float
remap (a, b) (a1, b1) x =  a1 + (b1 - a1) * (x - a) / (b - a)	

constrain2 :: (P2, P2) -> P2 -> P2
constrain2 ((xmin, ymin), (xmax, ymax)) (x, y) = (constrain (xmin, xmax) x, constrain (ymin, ymax) y)

constrain :: (Float, Float) -> Float -> Float
constrain (xmin, xmax) x = min xmax (max xmin x)