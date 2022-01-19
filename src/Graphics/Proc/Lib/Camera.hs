module Graphics.Proc.Lib.Camera(
  camera,
  camera2,
) where

import Control.Monad.Trans.State.Strict

import GHC.Float (float2Double)
import qualified Graphics.Rendering.OpenGL as G
import Graphics.Proc.Core

-- | Define a viewing transformation

-- > camera eye center up
--
-- @camera@ creates a viewing matrix derived from an eye point,
-- a reference point indicating the center of the scene, and an UP vector.
camera :: P3 -> P3 -> P3 -> Pio ()
camera (eyeX, eyeY, eyeZ) (centerX, centerY, centerZ) (upX, upY, upZ) =
  liftIO $ G.lookAt
    (G.Vertex3 (float2Double eyeX) (float2Double eyeY) (float2Double eyeZ))
    (G.Vertex3 (float2Double centerX) (float2Double centerY) (float2Double centerZ))
    (G.Vector3 (float2Double upX) (float2Double upY) (float2Double upZ))

-- | 2D camera view. It defines the center point, distance (affects scaling) and rotation
--
-- > camera2 center distance angle
camera2 :: P2 -> Float -> Float -> Pio ()
camera2 (cx, cy) dist angle = camera (cx, cy, dist) (cx, cy, 0) (cos angle, sin angle, 0)

