module Graphics.Proc.Core.State.Elements.Input(
    InputState(..), MouseButton(..), Modifiers(..), Key(..), KeyState(..),
    UpdateInput, saveKeyPress, saveMouseButton, savePosition
) where

import Data.Default 
import Control.Monad.Trans.State.Strict

import Graphics.Proc.Core.GLBridge

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

------------------------------------------------------

type UpdateInput = State InputState ()

saveKeyPress :: Key -> UpdateInput
saveKeyPress key = modify $ \x -> x { lastPressedKey = key }

saveMouseButton :: Maybe MouseButton -> UpdateInput
saveMouseButton mb = modify $ \x -> x { pressedButton = mb }

fromPosition (Position x y) = (fromEnum x, fromEnum y)

savePosition :: Position -> UpdateInput
savePosition pos = modify $ \x -> x { mousePosition = fromPosition pos }
