module Graphics.Proc.Lib.Environment(
	winSize, winWidth, winHeight
) where

import Graphics.Proc.Core

winSize :: Pio P2
winSize = liftA2 (,) winWidth winHeight

winWidth, winHeight :: Pio Float

winWidth  = liftIO $ fmap (fromIntegral . fst) getWindowSize
winHeight = liftIO $ fmap (fromIntegral . snd) getWindowSize

