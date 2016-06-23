{-# Language DeriveFunctor, GeneralizedNewtypeDeriving #-}
-- | The Processing IO-monad.
module Graphics.Proc.Pio(
	Pio, runPio, InputState(..), GlobalState(..), defGlobalState,
	MouseButton(..), Modifiers(..), Key(..), KeyState(..),

	Seed, 

	EllipseMode(..)
) where

import Data.Default
import Data.Time.Clock
import Data.Time.Calendar

import Control.Monad.IO.Class
import qualified System.Random as S
import System.Random hiding (random)

import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Strict as S

import Graphics.Proc.GLBridge

newtype Pio a = Pio { unPio :: StateT GlobalState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runPio :: Pio a -> GlobalState -> IO (a, GlobalState)
runPio (Pio x) st = runStateT x st

data GlobalState = GlobalState 
  { globalInputState    :: InputState
  , globalRandomGen     :: Maybe StdGen
  , globalNoiseGen      :: Maybe Int
  , globalNoiseDetails  :: NoiseDetails
  , globalFill          :: Maybe Col
  , globalStroke        :: Maybe Col   
  , globalEllipseMode   :: EllipseMode
  , globalLastTime      :: UTCTime
  , globalStartTime     :: UTCTime
  }

defGlobalState :: InputState -> IO GlobalState
defGlobalState inputSt = fmap (\x -> GlobalState inputSt Nothing Nothing def def def def x x) getCurrentTime

------------------------------------------------
-- input state

data InputState = InputState 
  { lastPressedKey   :: Key
  , pressedModifiers :: Modifiers
  , mousePosition    :: (Int, Int)
  , pressedButton    :: Maybe MouseButton
  }

instance Default Modifiers where
  def = Modifiers Up Up Up

instance Default InputState where
  def  = InputState 
    { lastPressedKey = Char ' '
    , pressedModifiers = def 
    , mousePosition   = (0, 0)
    , pressedButton   = Nothing
    }

--------------------------------------------
-- random values

type Seed = Maybe Int

data NoiseDetails = NoiseDetails 
  { noiseDetailsOctaves :: Int
  , noiseDetailsFalloff :: Float
  }

instance Default NoiseDetails where
  def = NoiseDetails 4 0.5

--------------------------------------------
-- drawing state

data EllipseMode = Radius | Center | Corner | Corners
  deriving (Show, Eq, Enum, Bounded)

instance Default EllipseMode where
  def = Center
