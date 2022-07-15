module Graphics.Proc.Lib3.Transform(
  translate,
  rotateX, rotateY, rotateZ,
  scale,
  resetMatrix,
  local,
) where

import Control.Monad.State.Strict

import Graphics.Rendering.OpenGL hiding (scale, translate, rotate)
import qualified Graphics.Rendering.OpenGL as G

import Graphics.Proc.Core

-- | Specifies an amount to displace objects within the display window. The x parameter specifies left/right translation, the y parameter specifies up/down translation
--
-- processing docs: <https://processing.org/reference/translate_.html>
translate :: P3 -> Draw
translate p = liftIO $ G.translate $ toVector p

-- | Rotates around given 3D vector.
rotateBy :: Vector3 GLfloat -> Float -> Draw
rotateBy v x = liftIO $ G.rotate (x * 360) v

-- | Rotates around X-axis.
rotateX :: Float -> Draw
rotateX = rotateBy $ Vector3 (1 :: GLfloat) 0 0

-- | Rotates around Y-axis.
rotateY :: Float -> Draw
rotateY = rotateBy $ Vector3 0 (1 :: GLfloat) 0

-- | Rotates around Z-axis.
rotateZ :: Float -> Draw
rotateZ = rotateBy $ Vector3 0 0 (1 :: GLfloat)

-- | Increases or decreases the size of a shape by expanding and contracting vertices. Objects always scale from their relative origin to the coordinate system. Scale values are specified as decimal percentages. For example, the function call scale(2.0) increases the dimension of a shape by 200%.
--
-- processing docs: <https://processing.org/reference/scale_.html>
scale :: P3 -> Draw
scale (P3 x y z) = liftIO $ G.scale x y z

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

