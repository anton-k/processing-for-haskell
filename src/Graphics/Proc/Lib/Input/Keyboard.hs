module Graphics.Proc.Lib.Input.Keyboard(
	key, modifiers
) where

import Graphics.Proc.Core	

key :: Pio Key
key = readPio lastPressedKey

modifiers :: Pio Modifiers
modifiers = readPio pressedModifiers
