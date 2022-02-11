module Graphics.Proc.Lib.Color(
  stroke, noStroke,
  fill, noFill,
  background, clear,
  strokeFill,

  rgb, grey, rgba, greya, setAlpha,
  hsv, hsva,

  white, black, navy, blue, aqua, teal, olive, green,
  lime, yellow, orange, red, maroon, fushsia, purple,
  gray, silver
) where

import Control.Monad.State.Strict
import Graphics.Rendering.OpenGL hiding(clear)
import qualified Graphics.Rendering.OpenGL as G
import qualified Data.Fixed as F

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
-- HSV color model

-- |
-- * Hue (H) is 0 to 360
-- * Saturation (S) is 0 to 1
-- * Value (V) or brightness is 0 to 1
hsv :: Float -> Float -> Float -> Col
hsv h s v = rgb r g b
  where
    !c = v * s
    !x = c * (1 - abs (((h / 60) `F.mod'` 2) - 1))
    !m = v - c

    (r', g', b')
      | hIn   0  60 = (c, x, 0)
      | hIn  60 120 = (x, c, 0)
      | hIn 120 180 = (0, c, x)
      | hIn 180 240 = (0, x, c)
      | hIn 240 300 = (x, 0, c)
      | otherwise   = (c, 0, x)

    hIn a b = a <= h && h < b
    fromRel a = (a + m) * 255
    !r = fromRel r'
    !g = fromRel g'
    !b = fromRel b'

hsva :: Float -> Float -> Float -> Float -> Col
hsva h s v a = setAlpha a $ hsv h s v

------------------------------------------------------

setAlpha :: Float -> Col -> Col
setAlpha x (Col r g b _) = Col r g b x

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
