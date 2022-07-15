module Graphics.Proc.Lib.Transform(
  translate,
  rotate,
  scale,
  resetMatrix,
  local,
  applyMatrix,
  shearX, shearY
) where

import Control.Monad.State.Strict

import Graphics.Rendering.OpenGL hiding (scale, translate, rotate)
import qualified Graphics.Rendering.OpenGL as G

import Graphics.Proc.Core

-- | Specifies an amount to displace objects within the display window. The x parameter specifies left/right translation, the y parameter specifies up/down translation
--
-- processing docs: <https://processing.org/reference/translate_.html>
translate :: P2 -> Draw
translate p = liftIO $ G.translate $ toVector (toP3 p)

-- | Rotates the amount specified by the angle parameter. Angles must be specified in taus (values from 0 to 1)
--
-- processing docs: <https://processing.org/reference/rotate_.html>
rotate :: Float -> Draw
rotate x = liftIO $ G.rotate (x * 360) (Vector3 0 0 (1 :: GLfloat))

-- | Increases or decreases the size of a shape by expanding and contracting vertices. Objects always scale from their relative origin to the coordinate system. Scale values are specified as decimal percentages. For example, the function call scale(2.0) increases the dimension of a shape by 200%.
--
-- processing docs: <https://processing.org/reference/scale_.html>
scale :: P2 -> Draw
scale (P2 x y) = liftIO $ G.scale x y 1

-- | Replaces the current matrix with the identity matrix. The equivalent function in OpenGL is glLoadIdentity().
--
-- processing docs: <https://processing.org/reference/resetMatrix_.html>
resetMatrix :: Draw
resetMatrix = liftIO $ loadIdentity

-- | Applies local transformation. Substitutes the pair of pushMatrix and popMatrix.
-- It can be used like this:
--
-- > local $ do
-- >   rotate angle
-- >   translate p1
-- >   drawShape params
--
-- see <https://processing.org/reference/pushMatrix_.html> and <https://processing.org/reference/popMatrix_.html>
local :: Draw -> Draw
local (Pio a) = Pio $ StateT $ \s -> do
    preservingMatrix $ do
        runStateT a s

-- | Multiplies the current matrix by the one specified through the parameters.
-- This is very slow because it will try to calculate the inverse of the transform, so avoid it whenever possible. The equivalent function in OpenGL is glMultMatrix().
--
-- processing docs: <https://processing.org/reference/applyMatrix_.html>
applyMatrix :: [Float] -> Draw
applyMatrix as@[a11, a12, a21, a22] =
    applyMatrix
        [ a11, a12,   0, 0
        , a21, a22,   0, 0
        ,   0,   0,   1, 0
        ,   0,   0,   0, 1]
applyMatrix as@[a11, a12, a13, a21, a22, a23, a31, a32, a33] =
    applyMatrix
        [ a11, a12, a13, 0
        , a21, a22, a23, 0
        , a31, a32, a33, 0
        ,   0,   0,   0, 1]
applyMatrix as@[a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44] = liftIO $ do
    m <- newMatrix RowMajor (fmap f2d as)
    multMatrix (m :: GLmatrix GLdouble)
applyMatrix _ = error "Wrong matrix size. The list should contain 4, 9 or 16 elements"

-- | Shears a shape around the x-axis the amount specified by the angle parameter. A
--
-- processing docs: <https://processing.org/reference/shearX_.html>
shearX :: Float -> Draw
shearX x = applyMatrix [1, x, 0, 1]

-- | Shears a shape around the y-axis the amount specified by the angle parameter. A
--
-- processing docs: <https://processing.org/reference/shearY_.html>
shearY :: Float -> Draw
shearY x = applyMatrix [1, 0, x, 1]

printMatrix :: Draw
printMatrix = undefined
