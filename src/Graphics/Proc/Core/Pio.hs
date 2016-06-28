{-# Language DeriveFunctor, GeneralizedNewtypeDeriving #-}
-- | The Processing IO-monad.
module Graphics.Proc.Core.Pio(
	Pio(..), runPio, readPio, readStatePio, modifyStatePio, 
  InputState(..), GlobalState(..), defGlobalState,
	MouseButton(..), Modifiers(..), Key(..), KeyState(..),
  FontSpec(..),
  
	Seed, 

	EllipseMode(..), NoiseDetails(..)
) where

import Data.Default
import Data.Time.Clock

import Control.Monad.IO.Class
import qualified System.Random as S
import System.Random hiding (random)

import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Strict as S

import Graphics.Proc.Core.GLBridge

newtype Pio a = Pio { unPio :: StateT GlobalState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runPio :: Pio a -> GlobalState -> IO (a, GlobalState)
runPio (Pio x) st = runStateT x st

readPio :: (InputState -> a) -> Pio a
readPio selector = readStatePio (selector . globalInputState)

readStatePio :: (GlobalState -> a) -> Pio a
readStatePio selector = Pio $ do
  st <- S.get  
  return $ selector st

modifyStatePio :: (GlobalState -> GlobalState) -> Pio ()
modifyStatePio update = Pio $ do
  st <- S.get
  S.put $ update st

data GlobalState = GlobalState 
  { globalInputState    :: InputState
  , globalRandomGen     :: Maybe StdGen
  , globalNoiseGen      :: Maybe Int
  , globalNoiseDetails  :: NoiseDetails
  , globalFill          :: Maybe Col
  , globalStroke        :: Maybe Col   
  , globalEllipseMode   :: EllipseMode
  , globalFont          :: Maybe FontSpec
  , globalFrameCount    :: Int
  , globalLastTime      :: UTCTime
  , globalStartTime     :: UTCTime  
  }

defGlobalState :: InputState -> IO GlobalState
defGlobalState inputSt = fmap (\x -> GlobalState inputSt Nothing Nothing def def def def def 0 x x) getCurrentTime

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

--------------------------------------------
-- fonts

data FontSpec = FontSpec 
  { fontCurrent   :: Font
  , fontInitSize  :: Int
  , fontSize      :: Int
  }

