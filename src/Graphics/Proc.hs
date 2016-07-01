-- | Processing primitves.
module Graphics.Proc(
	-- * Structure
	Proc(..), runProc,
	
	-- * Types
	Pio, Draw, Col(..), P2, P3,

	-- * Environment
	winSize, winWidth, winHeight,
	size,
	smooth, noSmooth, frameCount,

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
	EllipseMode, RectMode, DrawMode(..), ellipseMode, rectMode,
	strokeWeight,

	-- ** Vertex

	-- ** Loading & Displaying

	-- * Input

	-- ** Mouse
	mouse, mouseX, mouseY, 
	relMouse, relMouseX, relMouseY,

	-- ** Keyboard
	Key(..), key, Modifiers(..), modifiers, 

	-- ** Files

	-- ** Time & Date
	year, month, day, hour, minute, second, millis, utcHour,

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

	-- ** Coordinates

    -- ** Material Properties  
	
	-- * Color		
    fill, noFill, stroke, noStroke, strokeFill,
    rgb, rgba, grey, greya, 
	background, clear,

	white, black, green, blue, orange, yellow, red,

	-- * Image	

	-- ** Loading & Displaying

	-- ** Textures

	-- ** Pixels

	-- ** Rendering

	-- ** Shaders

	-- * Typography

	-- ** Loading & Displaying
	-- Font, loadFont, text, textFont,

	-- ** Attributes
	-- textSize,
	
	-- ** Metrics

	-- * Math

	-- ** Operators
	
	-- ** Bitwise Operators
	
	-- ** Calculation
	remap, FloatInterval,
	constrain, constrain2,

	-- ** Trigonometry
	radians, degrees, e, erad,

	-- ** Random
	randomSeed, random, random2, randomP2, randomCol, 
	NoiseDetails(..), noiseSeed, noise1, noise2, noise3,
	randomGaussian,

	-- * Misc
	onCircle, onLine, uon,

	-- * Pio mutable values
	PioRef, newPioRef, readPioRef, writePioRef, modifyPioRef,

	-- | Useful standard functions
	module Data.VectorSpace,
	module Data.AffineSpace,
	module Data.Cross,	
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
import Data.AffineSpace
import Data.Cross

import Graphics.Proc.Core
import Graphics.Proc.Lib
