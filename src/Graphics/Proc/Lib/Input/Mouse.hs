module Graphics.Proc.Lib.Input.Mouse(
	mouse, mouseX, mouseY, 
	relMouse, relMouseX, relMouseY
) where

import Graphics.Proc.Core
import Graphics.Proc.Lib.Environment

mouse :: Pio P2
mouse = readPio ((\(x, y) -> (fromIntegral x, fromIntegral y)) . mousePosition)

mouseX, mouseY :: Pio Float

mouseX = fmap fst mouse
mouseY = fmap snd mouse

relMouseX, relMouseY :: Pio Float 

relMouseX = do
  mx <- mouseX
  w  <- winWidth
  return $ mx / w

relMouseY = do
  my <- mouseY
  h  <- winHeight
  return $ my / h

relMouse :: Pio P2
relMouse = liftA2 (,) relMouseX relMouseY
