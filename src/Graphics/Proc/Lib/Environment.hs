module Graphics.Proc.Lib.Environment(
	winSize, winWidth, winHeight,
	size,
	smooth, noSmooth, 
    frameCount, frameRate
) where

import qualified Control.Monad.Trans.State.Strict as S
import Graphics.Rendering.OpenGL

import Graphics.Proc.Core

winSize :: Pio P2
winSize = liftA2 (,) winWidth winHeight

winWidth, winHeight :: Pio Float

winWidth  = liftIO $ fmap (fromIntegral . fst) getWindowSize
winHeight = liftIO $ fmap (fromIntegral . snd) getWindowSize

--------------------------------------------

size :: P2 -> Pio ()
size = liftIO . glSize

--------------------------------------------

smooth :: Pio ()
smooth = liftIO $ pointSmooth $= Enabled

noSmooth :: Pio ()
noSmooth = liftIO $ pointSmooth $= Disabled

frameRate :: Float -> Pio ()
frameRate = putFrameRate
