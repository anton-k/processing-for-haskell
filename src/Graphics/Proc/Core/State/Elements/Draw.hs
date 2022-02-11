module Graphics.Proc.Core.State.Elements.Draw(
    DrawState(..),
    EllipseMode, RectMode, DrawMode(..),
    StrokeCap(..), StrokeJoin(..)
) where

import Control.Monad.State.Strict
import Data.Default
import Graphics.Proc.Core.GLBridge

data DrawState = DrawState
  { drawEllipseMode   :: EllipseMode
  , drawRectMode      :: RectMode
  , drawStrokeCap     :: StrokeCap
  , drawStrokeJoin    :: StrokeJoin
  , drawStrokeWeight  :: Float
  , drawFill          :: Maybe Col
  , drawStroke        :: Maybe Col
  }

instance Default DrawState where
  def = DrawState
    { drawEllipseMode  = Center
    , drawRectMode     = Corner
    , drawStrokeCap    = Round
    , drawStrokeJoin   = JoinMiter
    , drawStrokeWeight = 1
    , drawFill         = Just $ Col 0 0 0 1
    , drawStroke       = Just $ Col 0 0 0 1
    }

-- | Modes for drawing of ellipse. See @ellipseMode@.
type EllipseMode = DrawMode

-- | Modes for drawing of rectangle. See @rectMode@.
type RectMode = DrawMode

-- | Modes for drawing of rectangle or ellipse.
data DrawMode = Radius | Center | Corner | Corners
  deriving (Show, Eq, Enum, Bounded)

instance Default DrawMode where
  def = Center

data StrokeCap = Round | Square | Project
  deriving (Show, Eq, Enum, Bounded)

instance Default StrokeCap where
  def = Round

data StrokeJoin = JoinMiter | JoinBevel | JoinRound
  deriving (Show, Eq, Enum, Bounded)

instance Default StrokeJoin where
  def = JoinMiter

----------------------------------------------------------
