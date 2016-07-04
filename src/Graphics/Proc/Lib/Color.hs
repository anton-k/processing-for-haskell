module Graphics.Proc.Lib.Color(
	stroke, noStroke,
	fill, noFill,
	background, clear,
	strokeFill,

	rgb, grey, rgba, greya,

	white, black, navy, blue, aqua, teal, olive, green,
	lime, yellow, orange, red, maroon, fushsia, purple,
	gray, silver
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

-- | White color.
white :: Col
white = Col 1 1 1 1

-- | Black color.
black :: Col
black = Col 0 0 0 1

-- | Nave color.
navy :: Col
navy = rgb 0 31 63

-- | Blue color.
blue :: Col
blue = rgb 0 116 217

-- | Aqua color.
aqua :: Col
aqua = rgb 127 219 255

-- | Teal color.
teal :: Col
teal = rgb 57 204 204

-- | Olive color.
olive :: Col
olive = rgb 61 153 112

-- | Green color.
green :: Col
green = rgb 46 204 64

-- | Lime color.
lime :: Col
lime = rgb 1 255 112

-- | Yellow color.
yellow :: Col
yellow = rgb 255 220 0

-- | Orange color
orange :: Col 
orange = rgb 255 33 27

-- | Red color
red :: Col
red = rgb 255 65 54

-- | Maroon color.
maroon :: Col
maroon = rgb 133 20 75

-- | Fuchsia color.
fushsia :: Col
fushsia = rgb 240 18 190

-- | Purple color
purple :: Col
purple = rgb 177 13 201

-- | Gray color.
gray :: Col
gray = grey 170

-- | Silver color.
silver :: Col
silver = grey 221