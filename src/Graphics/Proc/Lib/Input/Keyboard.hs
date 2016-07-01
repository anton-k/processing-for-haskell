module Graphics.Proc.Lib.Input.Keyboard(
	key, modifiers
) where

import Graphics.Proc.Core	

key :: Pio Key
key = getLastPressedKey

modifiers :: Pio Modifiers
modifiers = getPressedModifiers
