module Graphics.Proc.Lib.Shape.Attribute(
	EllipseMode(..), ellipseMode, rectMode,
	strokeWeight
) where

import Control.Monad.Trans.State.Strict
import Graphics.Rendering.OpenGL

import Graphics.Proc.Core

ellipseMode :: EllipseMode -> Draw
ellipseMode = putEllipseMode

rectMode :: RectMode -> Draw
rectMode = putRectMode

strokeWeight :: Float -> Draw
strokeWeight x = liftIO $ lineWidth $= x


