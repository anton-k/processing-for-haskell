-- | Processing primitves.
module Graphics.Proc(
	-- * Structure
	Proc(..), runProc,
	
	-- * Types
	Pio, Draw, Col(..), P2, P3,

	EllipseMode(..),

	-- * Environment

	-- * Data
	
	-- * Control
	
	-- * Shape

	-- ** 2D Primitives

	triangle, rect, quad, ellipse, circle, line, linePath, point, pointPath, polygon,

	-- ** Curves

	-- bezier, 

	-- * Input

	-- ** Mouse
	mouse, mouseX, mouseY, 
	relMouse, relMouseX, relMouseY,

	-- ** Keyboard
	key, modifiers,

	-- ** Files

	-- ** Time & Date
	year, month, day, hour, minute, second, millis,

	-- * Output	
	
	-- * Transform
	translate, 
	rotate, rotateX, rotateY, rotateZ, 
	scale, 
	resetMatrix, local, 
	applyMatrix, 
	shearX, shearY,

	-- * Lights
	
	-- * Camera
	
	-- * Color		
    fill, noFill, stroke, noStroke, strokeFill,
    rgb, rgba, grey, greya, 
	background, clear,

	-- * Image	

	-- * Typography

	-- * Math

	-- | Useful standard functions
	module Data.VectorSpace,
    module Data.NumInstances,
	module Data.Default,
	module Data.Monoid,
	module Control.Monad,
	module Control.Applicative
) where

import Data.Default
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.VectorSpace hiding (Sum(..))
import Data.NumInstances

import Graphics.Proc.Core
import Graphics.Proc.Lib
