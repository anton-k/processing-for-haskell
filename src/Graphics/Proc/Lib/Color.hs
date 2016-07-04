module Graphics.Proc.Lib.Color(
	stroke, noStroke,
	fill, noFill,
	background, clear,
	strokeFill,

	rgb, grey, rgba, greya,

	white, black, green, blue, orange, yellow, red
) where

import Control.Monad.Trans.State.Strict
import Graphics.Rendering.OpenGL hiding(clear)
import qualified Graphics.Rendering.OpenGL as G

import Graphics.Proc.Core

-- | Sets the color used to draw lines and borders around shapes. 
--
-- processing docs: <https://processing.org/reference/stroke_.html>	
stroke :: Col -> Draw
stroke = putStroke . Just

-- | Disables drawing the stroke (outline). If both noStroke() and noFill() are called, nothing will be drawn to the screen
-- 
-- processing docs: <https://processing.org/reference/noStroke_.html>	
noStroke :: Draw
noStroke = putStroke Nothing

-- | Sets the color used to fill shapes. For example, if you run @fill (rgb 204 102 0)@, all subsequent shapes will be filled with orange. 
--
-- processing docs: <https://processing.org/reference/fill_.html>	
fill :: Col -> Draw
fill = putFill . Just

-- | Disables filling geometry. If both noStroke() and noFill() are called, nothing will be drawn to the screen. 
-- 
-- processing docs: <https://processing.org/reference/noFill_.html>	
noFill :: Draw
noFill = putFill Nothing

-- | Sets stroke and fill to the same color.
strokeFill :: Col -> Draw
strokeFill col = do
    stroke col 
    fill col

------------------------------------------------------

-- | The background() function sets the color used for the background of the Processing window. The default background is light gray. This function is typically used within draw() to clear the display window at the beginning of each frame, but it can be used inside setup() to set the background on the first frame of animation or if the backgound need only be set once. 
--
-- processing docs: <https://processing.org/reference/background_.html>	
background :: Col -> Draw
background x = liftIO $ do
  clearColor $= glCol x
  G.clear [ColorBuffer]      

-- | Clears the pixels within a buffer.
--
-- processing docs: <https://processing.org/reference/clear_.html>	

clear :: Draw
clear = liftIO $ do
	G.clear [ColorBuffer]      

------------------------------------------------------

-- | Creates an RGB-color from three values. The values are from 0 to 255.
rgb :: Float -> Float -> Float -> Col
rgb r g b = rgba r g b 255

-- | Creates a grey value out of single float value. The value is from 0 to 255.
grey :: Float -> Col
grey g = rgb g g g

-- | Creates an RGB-color with transparency.
rgba :: Float -> Float -> Float -> Float -> Col
rgba r g b a = Col (r / 255) (g / 255) (b / 255) (a / 255)

-- | Creates an grey-color with transparency.
greya :: Float -> Float -> Col
greya g a = rgba g g g a

------------------------------------------------------

white = Col 1 1 1 1
black = Col 0 0 0 1

red = Col 1 0 0 1
green = Col 0 1 0 1
blue = Col 0 0 1 1
orange = Col 1 1 0 1
yellow = Col 1 0 1 1
