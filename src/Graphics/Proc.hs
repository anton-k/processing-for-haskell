-- | Processing primitves.
module Graphics.Proc(
	Proc(..), runProc,
	Pio,

	Col(..),

	EllipseMode(..),

	module Data.Default,
	module Control.Monad,
	module Control.Applicative
) where

import Data.Default
import Control.Monad
import Control.Applicative

import Graphics.Proc.Core.Pio
import Graphics.Proc.Core.Run
import Graphics.Proc.Core.GLBridge
