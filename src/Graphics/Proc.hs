-- | Processing primitves.
module Graphics.Proc(
	-- * Structure
	Proc(..), runProc,
	
	-- * Types
	Pio, Draw, Col(..), P2, P3,

	-- * Environment
	winSize, winWidth, winHeight,
	size,
	smooth, noSmooth,

	-- * Data

	-- ** Primitive

	-- ** Composite

	-- ** Conversion
	float, int,
	
	-- ** String Functions

	-- ** Array Functions
	
	-- * Control

	-- ** Relational Operators
	-- ** Iteration
	-- ** Conditionals
	-- ** Logical Operators
	
	-- * Shape

	-- ** 2D Primitives

	triangle, rect, quad, ellipse, circle, line, linePath, point, pointPath, polygon,

	-- ** Curves
	bezier,

	-- ** 3D Primitives

	-- ** Attributes
	EllipseMode(..), ellipseMode,
	strokeWeight,

	-- ** Vertex

	-- ** Loading & Displaying

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

	-- ** Text Area
	println,

	-- ** Image
	
	-- ** Files
	
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
