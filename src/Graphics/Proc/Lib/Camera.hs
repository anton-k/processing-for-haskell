module Graphics.Proc.Lib.Camera(
  camera,
  camera2,
  ortho,
  frustrum,
  perspective,
) where

import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as G
import Graphics.Proc.Core

-- | Define a viewing transformation

-- > camera eye center up
--
-- @camera@ creates a viewing matrix derived from an eye point,
-- a reference point indicating the center of the scene, and an UP vector.
camera :: P3 -> P3 -> P3 -> Pio ()
camera eye center up =
  liftIO $ G.lookAt (toVertex eye) (toVertex center) (toVector up)

-- | 2D camera view. It defines the center point, distance (affects scaling) and rotation
--
-- > camera2 center distance angle
camera2 :: P2 -> Float -> Float -> Pio ()
camera2 (P2 cx cy) dist angle = camera (P3 cx cy dist) (P3 cx cy 0) (P3 (cos angle) (sin angle) 0)

ortho :: Float -> Float -> Float -> Float -> Float -> Float -> Pio ()
ortho left right bottom top near far =
  liftIO $ G.ortho (f2d left) (f2d right) (f2d bottom) (f2d top) (f2d near) (f2d far)

frustrum :: Float -> Float -> Float -> Float -> Float -> Float -> Pio ()
frustrum left right bottom top near far =
  liftIO $ G.ortho (f2d left) (f2d right) (f2d bottom) (f2d top) (f2d near) (f2d far)

perspective :: Float -> Float -> Float -> Float -> Pio ()
perspective fovy aspect zNear zFar =
  liftIO $ G.perspective (f2d fovy) (f2d aspect) (f2d zNear) (f2d zFar)
