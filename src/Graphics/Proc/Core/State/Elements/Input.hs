module Graphics.Proc.Core.State.Elements.Input(
    InputState(..), MouseButton(..), Modifiers(..), Key(..), KeyState(..)   
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
