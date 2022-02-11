module Graphics.Proc.Lib.Input.Keyboard(
  key, modifiers
) where

import Graphics.Proc.Core

-- | Returns last pressed key.
--
-- processing docs: <https://processing.org/reference/key.html>
key :: Pio Key
key = getLastPressedKey

-- | Returns last pressed key modifier.
modifiers :: Pio Modifiers
modifiers = getPressedModifiers
