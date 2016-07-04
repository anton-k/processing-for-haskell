module Graphics.Proc.Core.Vector(
	P2, P3,

	module Data.VectorSpace,
	module Data.NumInstances

) where

import Data.VectorSpace
import Data.NumInstances

-- | 2D vector.
type P2 = (Float, Float)

-- | 3D vector.
type P3 = (Float, Float, Float)
