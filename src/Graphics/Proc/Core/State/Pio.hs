{-# Language DeriveFunctor, GeneralizedNewtypeDeriving #-}
-- | The Processing IO-monad.
module Graphics.Proc.Core.State.Pio(
  Pio(..), runPio,
  GlobalState(..), defGlobalState, SphereRes(..), RawSphereRes(..), toRawSphereRes,

  onInput, onRnd, onDraw, onFont, onFrame, onTime, onTimeIO,
  module X
) where

import Data.Default
import Data.IORef
import Control.Monad.IO.Class     as X
import Control.Monad.State.Strict as X
import GHC.Int

import Graphics.Proc.Core.State.Elements

-- | Processing IO-monad. It has the same meaning as the Haskell IO-monad but
-- it's augmented with Processing library functions.
--
-- We can use @liftIO@ to execute ordinary Haskell IO-actions.
-- The Pio has instance for class @MonadIO@.
--
-- > text <- liftIO $ readFile filename
newtype Pio a = Pio { unPio :: StateT GlobalState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState GlobalState)

runPio :: Pio a -> GlobalState -> IO (a, GlobalState)
runPio (Pio x) st = runStateT x st

readPio :: (InputState -> a) -> Pio a
readPio selector = readStatePio (selector . globalInputState)

readStatePio :: (GlobalState -> a) -> Pio a
readStatePio selector = Pio $ do
  st <- get
  return $ selector st

modifyStatePio :: (GlobalState -> GlobalState) -> Pio ()
modifyStatePio update = Pio $ do
  st <- get
  put $ update st

data GlobalState = GlobalState
  { globalInputState    :: !InputState
  , globalRndState      :: !RndState
  , globalDrawState     :: !DrawState
  , globalFontState     :: !FontState
  , globalTimeState     :: !TimeState
  , globalFrameState    :: !FrameState
  , globalSphereDetail  :: !RawSphereRes
  }

data RawSphereRes = RawSphereRes
  { rawSphereRes'longitude :: !Int32
  , rawSphereRes'latitude  :: !Int32
  }

-- | Sphere resolution
data SphereRes = SphereRes
  { sphereRes'longitude :: !Int
  , sphereRes'latitude  :: !Int
  }

toRawSphereRes :: SphereRes -> RawSphereRes
toRawSphereRes (SphereRes a b) = RawSphereRes (fromIntegral a) (fromIntegral b)

instance Default SphereRes where
  def = SphereRes 30 30

instance Default RawSphereRes where
  def = toRawSphereRes def
-- | Hack to construct resolution from single number

instance Num SphereRes where
  fromInteger n = SphereRes (fromInteger n) (fromInteger n)
  (+) = undefined
  (*) = undefined
  (-) = undefined
  negate = undefined
  abs = undefined
  signum = undefined

defGlobalState :: IO GlobalState
defGlobalState = fmap (\timeSt -> GlobalState def def def def timeSt def def) initTimeState

onInput :: State InputState a -> Pio a
onInput = onState globalInputState (\x a -> x { globalInputState = a })

onRnd :: State RndState a -> Pio a
onRnd = onState globalRndState (\x a -> x { globalRndState = a })

onDraw :: State DrawState a -> Pio a
onDraw = onState globalDrawState (\x a -> x { globalDrawState = a })

onFont :: State FontState a -> Pio a
onFont = onState globalFontState (\x a -> x { globalFontState = a })

onFrame :: State FrameState a -> Pio a
onFrame = onState globalFrameState (\x a -> x { globalFrameState = a })

onTime :: State TimeState a -> Pio a
onTime = onState globalTimeState (\x a -> x { globalTimeState = a })

onTimeIO :: StateT TimeState IO a -> Pio a
onTimeIO = onStateIO globalTimeState (\x a -> x { globalTimeState = a })

--------------------------------------------------

onState :: (GlobalState -> a) -> (GlobalState -> a -> GlobalState) -> State a b -> Pio b
onState getter setter act = Pio $ do
  st <- get
  let (b, a) = runState act (getter st)
  put $ setter st a
  return b

onStateIO :: (GlobalState -> a) -> (GlobalState -> a -> GlobalState) -> StateT a IO b -> Pio b
onStateIO getter setter act = Pio $ do
  st <- get
  (b, a) <- liftIO $ runStateT act (getter st)
  put $ setter st a
  return b
