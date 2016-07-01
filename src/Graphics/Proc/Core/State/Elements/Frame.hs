module Graphics.Proc.Core.State.Elements.Frame(
    FrameState(..), LoopMode(..),
    updateFrameCount
) where

import Control.Monad.Trans.State.Strict
import Data.Default 

data FrameState = FrameState 
  { frameCount :: Int
  , frameRate  :: Int
  , frameLoop  :: LoopMode
  }

instance Default FrameState where
  def = FrameState 0 60 Loop

data LoopMode = Loop | NoLoop | Redraw
  deriving (Show, Eq, Enum, Bounded)

instance Default LoopMode where
    def = Loop  

updateFrameCount :: State FrameState ()
updateFrameCount = modify $ \x -> x { frameCount = succ (frameCount x) }
