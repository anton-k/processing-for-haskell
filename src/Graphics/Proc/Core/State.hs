module Graphics.Proc.Core.State(
	Pio(..), runPio, 
  	GlobalState(..), defGlobalState,

	-- * Input
	MouseButton(..), Modifiers(..), Key(..), KeyState(..),
	putKeyPress, putMouseButton, putPosition,
	getMousePosition, getLastPressedKey, getPressedModifiers,

	-- * Random
	NoiseDetails(..), Seed,
	getRandomGen, getNoiseGen, getNoiseDetails, 
	putRandomGen, putNoiseGen, putNoiseDetails,
	putOctaves,

	-- * Draw
	DrawState(..),
    EllipseMode, RectMode, DrawMode(..), 
    StrokeCap(..), StrokeJoin(..),

    getStroke, getFill, getEllipseMode, getRectMode,
    putEllipseMode, putStroke, putFill, putRectMode,

	-- * Font

	-- * Frame
	LoopMode(..), updateFrameCount, frameCount,

	-- * Time
	initTimeState, TimeInterval, getDuration, getStartTime
) where

import Data.Time.Clock
import Control.Monad.Trans.State.Strict

import Graphics.Proc.Core.State.Pio
import Graphics.Proc.Core.GLBridge
import Graphics.Proc.Core.Vector
import qualified Graphics.Proc.Core.State.Elements as E
import Graphics.Proc.Core.State.Elements hiding (updateFrameCount, getDuration, frameCount)

----------------------------------------
-- input

putKeyPress :: Key -> Pio ()
putKeyPress key = onInput $ modify $ \x -> x { lastPressedKey = key }

putMouseButton :: Maybe MouseButton -> Pio ()
putMouseButton mb = onInput $ modify $ \x -> x { pressedButton = mb }

putPosition :: Position -> Pio ()
putPosition pos = onInput $ modify $ \x -> x { mousePosition = fromPosition pos }
	where fromPosition (Position x y) = (fromEnum x, fromEnum y)

getMousePosition :: Pio P2
getMousePosition = onInput $ fmap ((\(x, y) -> (fromIntegral x, fromIntegral y)) . mousePosition) get

getLastPressedKey :: Pio Key
getLastPressedKey   = onInput $ fmap lastPressedKey get

getPressedModifiers :: Pio Modifiers
getPressedModifiers = onInput $ fmap pressedModifiers get

----------------------------------------
-- random

getRandomGen     = onRnd $ fmap rndRandomGen get
getNoiseGen      = onRnd $ fmap rndNoiseGen  get
getNoiseDetails  = onRnd $ fmap rndNoiseDetails get

putRandomGen     v = onRnd $ modify $ \x -> x { rndRandomGen = v }
putNoiseGen      v = onRnd $ modify $ \x -> x { rndNoiseGen = v }
putNoiseDetails  v = onRnd $ modify $ \x -> x { rndNoiseDetails = v }

putOctaves v = onRnd $ modify $ \x -> x { rndNoiseDetails = go (rndNoiseDetails x) v }
	where go nd v = nd { noiseDetailsOctaves = v }

----------------------------------------
-- draw

getStroke :: Pio (Maybe Col)
getStroke = fmap drawStroke $ onDraw get

getFill :: Pio (Maybe Col)
getFill = fmap drawFill $ onDraw get

getEllipseMode :: Pio EllipseMode
getEllipseMode = fmap drawEllipseMode $ onDraw get

getRectMode :: Pio RectMode
getRectMode = fmap drawRectMode $ onDraw get

putEllipseMode :: EllipseMode -> Pio ()
putEllipseMode value = onDraw $ modify $ \x -> x { drawEllipseMode = value }

putRectMode :: RectMode -> Pio ()
putRectMode value = onDraw $ modify $ \x -> x { drawRectMode = value }

putStroke :: Maybe Col -> Pio ()
putStroke value = onDraw $ modify $ \x -> x { drawStroke = value }

putFill :: Maybe Col -> Pio ()
putFill value = onDraw $ modify $ \x -> x { drawFill = value }

----------------------------------------
-- font

----------------------------------------
-- frame

updateFrameCount :: Pio ()
updateFrameCount = onFrame E.updateFrameCount

frameCount :: Pio Int
frameCount = onFrame $ fmap E.frameCount get

----------------------------------------
-- time

getDuration :: Pio TimeInterval
getDuration = onTimeIO E.getDuration

getStartTime :: Pio UTCTime
getStartTime = onTime $ fmap timeStart get