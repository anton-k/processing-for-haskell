module Graphics.Proc.Lib.Input.Mouse(
  mouse, mouseX, mouseY,
  relMouse, relMouseX, relMouseY,
  mouseButton
) where

import Graphics.Proc.Core
import Graphics.Proc.Lib.Environment

-- | Contains coordinates of the mouse as a vector.
mouse :: Pio P2
mouse = getMousePosition

-- | The system variable mouseX always contains the current horizontal coordinate of the mouse.
--
-- processing docs: <https://processing.org/reference/mouseX.html>
mouseX :: Pio Float
mouseX = fmap px mouse

-- | The system variable mouseX always contains the current vertical coordinate of the mouse.
--
-- processing docs: <https://processing.org/reference/mouseY.html>
mouseY :: Pio Float
mouseY = fmap py mouse

relMouseX, relMouseY :: Pio Float

-- | Contains relative @mouseX@ coordinates of the mouse (scaled to the interval [0, 1]).
relMouseX = do
  mx <- mouseX
  w  <- winWidth
  return $ mx / w

-- | Contains relative @mouseY@ coordinates of the mouse (scaled to the interval [0, 1]).
relMouseY = do
  my <- mouseY
  h  <- winHeight
  return $ my / h

-- | Contains relative coordinates of the mouse as a vector.
relMouse :: Pio P2
relMouse = liftA2 P2 relMouseX relMouseY

mouseButton :: Pio (Maybe MouseButton)
mouseButton = getMouseButton
