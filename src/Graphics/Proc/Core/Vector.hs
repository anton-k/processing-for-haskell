module Graphics.Proc.Core.Vector(
	P2, P3,

	module Data.VectorSpace,
	module Data.NumInstances

) where

import Data.VectorSpace
import Data.NumInstances

type P2 = (Float, Float)
type P3 = (Float, Float, Float)
