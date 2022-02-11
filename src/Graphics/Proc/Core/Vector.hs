module Graphics.Proc.Core.Vector(
  P2(..), P3(..), IsPoint(..),
  module X
) where

import Data.VectorSpace  as X
import Data.NumInstances as X
import Data.AffineSpace  as X
import Data.Cross        as X

-- | 2D vector.
data P2 = P2 !Float !Float

-- | 3D vector.
data P3 = P3 !Float !Float !Float

--------------------------------------------------------------------------------

class IsPoint a where
  toP3 :: a -> P3
  px :: a -> Float
  py :: a -> Float
  pz :: a -> Float

instance IsPoint P3 where
  toP3 = id
  px (P3 x _ _) = x
  py (P3 _ y _) = y
  pz (P3 _ _ z) = z

instance IsPoint P2 where
  toP3 (P2 x y) = P3 x y 0
  px (P2 x _) = x
  py (P2 _ y) = y
  pz (P2 _ _) = 0

--------------------------------------------------------------------------------
-- Num

instance Num P2 where
  fromInteger n = P2 (fromInteger n) (fromInteger n)
  (+) (P2 x1 y1) (P2 x2 y2) = P2 (x1 + x2) (y1 + y2)
  (*) (P2 x1 y1) (P2 x2 y2) = P2 (x1 * x2) (y1 * y2)
  (-) (P2 x1 y1) (P2 x2 y2) = P2 (x1 - x2) (y1 - y2)
  negate (P2 x y) = P2 (negate x) (negate y)
  abs (P2 x y) = P2 (abs x) (abs y)
  signum (P2 x y) = P2 (signum x) (signum y)

instance Num P3 where
  fromInteger n = P3 (fromInteger n) (fromInteger n) (fromInteger n)
  (+) (P3 x1 y1 z1) (P3 x2 y2 z2) = P3 (x1 + x2) (y1 + y2) (z1 + z2)
  (*) (P3 x1 y1 z1) (P3 x2 y2 z2) = P3 (x1 * x2) (y1 * y2) (z1 * z2)
  (-) (P3 x1 y1 z1) (P3 x2 y2 z2) = P3 (x1 - x2) (y1 - y2) (z1 - z2)
  negate (P3 x y z) = P3 (negate x) (negate y) (negate z)
  abs (P3 x y z) = P3 (abs x) (abs y) (abs z)
  signum (P3 x y z) = P3 (signum x) (signum y) (signum z)

instance Fractional P2 where
  fromRational x = P2 (fromRational x) (fromRational x)
  recip (P2 x y) = P2 (recip x) (recip y)

instance Fractional P3 where
  fromRational x = P3 (fromRational x) (fromRational x) (fromRational x)
  recip (P3 x y z) = P3 (recip x) (recip y) (recip z)

--------------------------------------------------------------------------------
-- Vector space

instance AdditiveGroup P2 where
  zeroV = P2 0 0
  (^+^) a b = a + b
  (^-^) a b = a - b
  negateV = negate

instance AdditiveGroup P3 where
  zeroV = P3 0 0 0
  (^+^) a b = a + b
  (^-^) a b = a - b
  negateV = negate

instance VectorSpace P2 where
  type Scalar P2 = Float
  (*^) k (P2 x y) = P2 (k * x) (k * y)

instance VectorSpace P3 where
  type Scalar P3 = Float
  (*^) k (P3 x y z) = P3 (k * x) (k * y) (k * z)

instance AffineSpace P2 where
  type Diff P2 = P2
  (.-.) = (-)
  (.+^) = (+)

instance AffineSpace P3 where
  type Diff P3 = P3
  (.-.) = (-)
  (.+^) = (+)

instance HasNormal P2 where
  normalVec p = (recip len) *^ p
    where
      len = sqrt (p <.> p)

instance HasNormal P3 where
  normalVec p = (recip len) *^ p
    where
      len = sqrt (p <.> p)

instance HasCross2 P2 where
  cross2 (P2 x y) = P2 (negateV y) x

instance HasCross3 P3 where
  cross3 (P3 ax ay az) (P3 bx by bz)
    = P3 (ay * bz - az * by)
         (az * bx - ax * bz)
         (ax * by - ay * bx)

instance InnerSpace P2 where
  (<.>) (P2 x1 y1) (P2 x2 y2) = x1 * x2 + y1 * y2

instance InnerSpace P3 where
  (<.>) (P3 x1 y1 z1) (P3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2



