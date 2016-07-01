module Graphics.Proc.Core.State.Elements.Font(
    FontState(..)
) where

import Data.Default

data FontState = FontState
  { fontCurrent   :: Maybe Font
  , fontInitSize  :: Int
  , fontSize      :: Int
  }

type Font = String

instance Default FontState where
  def = FontState Nothing 12 12
