module Graphics.Proc.Lib.Shape.Attribute(
	EllipseMode(..), ellipseMode,
	strokeWeight
) where

import Control.Monad.Trans.State.Strict
import Graphics.Rendering.OpenGL

import Graphics.Proc.Core

ellipseMode :: EllipseMode -> Draw
ellipseMode mode = Pio $ modify $ \s -> s { globalEllipseMode = mode }

strokeWeight :: Float -> Draw
strokeWeight x = liftIO $ lineWidth $= x


