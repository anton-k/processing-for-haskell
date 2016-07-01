module Graphics.Proc.Core.State(
	Pio, runPio, 
  	GlobalState(..), defGlobalState,

	-- * Input
	MouseButton(..), Modifiers(..), Key(..), KeyState(..),
	saveKeyPress, saveMouseButton, savePosition,

	-- * Random
	NoiseDetails(..), Seed,

	-- * Draw

	-- * Font

	-- * Frame
	LoopMode(..), updateFrameCount,

	-- * Time
	initTimeState, TimeInterval, getDuration
) where

import Graphics.Proc.Core.State.Pio
import Graphics.Proc.Core.GLBridge
import qualified Graphics.Proc.Core.State.Elements as E

import Graphics.Proc.Core.State.Elements hiding(
		saveKeyPress, saveMouseButton, savePosition,
		updateFrameCount, getDuration)

----------------------------------------
-- input

saveKeyPress :: Key -> Pio ()
saveKeyPress = onInput . E.saveKeyPress

saveMouseButton :: Maybe MouseButton -> Pio ()
saveMouseButton = onInput . E.saveMouseButton

savePosition :: Position -> Pio ()
savePosition = onInput . E.savePosition

----------------------------------------
-- random

----------------------------------------
-- draw

----------------------------------------
-- font

----------------------------------------
-- frame

updateFrameCount :: Pio ()
updateFrameCount = onFrame E.updateFrameCount

----------------------------------------
-- time

getDuration :: Pio TimeInterval
getDuration = onTimeIO E.getDuration
