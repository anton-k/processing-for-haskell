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

stroke :: Col -> Draw
stroke = putStroke . Just

noStroke :: Draw
noStroke = putStroke Nothing

fill :: Col -> Draw
fill = putFill . Just

noFill :: Draw
noFill = putFill Nothing

strokeFill :: Col -> Draw
strokeFill col = do
    stroke col 
    fill col

------------------------------------------------------

background :: Col -> Draw
background x = liftIO $ do
  clearColor $= glCol x
  G.clear [ColorBuffer]      

clear :: Draw
clear = liftIO $ do
	G.clear [ColorBuffer]      

------------------------------------------------------

greyCol :: Float -> Col
greyCol x = Col x x x 1

rgb :: Float -> Float -> Float -> Col
rgb r g b = rgba r g b 255

grey :: Float -> Col
grey g = rgb g g g

rgba :: Float -> Float -> Float -> Float -> Col
rgba r g b a = Col (r / 255) (g / 255) (b / 255) (a / 255)

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
