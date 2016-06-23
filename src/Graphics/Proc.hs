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

import Graphics.Proc.Pio
import Graphics.Proc.Run
import Graphics.Proc.GLBridge
