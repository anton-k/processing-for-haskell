module Graphics.Proc.Core.GLBridge(
	MouseButton(..), Modifiers(..), Key(..), KeyState(..),
	Col(..), glCol,
  f2d, v2, p2v,
	glSize, setupWindow, getWindowSize
) where

import Graphics.Rendering.OpenGL hiding (scale, translate, rotate, rect, height, width)
import qualified Graphics.Rendering.OpenGL as G
import Graphics.UI.GLUT hiding (scale, translate, rotate, rect, rgba)

import Control.Monad.IO.Class
import GHC.Float

data Col = Col Float Float Float Float
    deriving (Show)

glCol (Col r g b a) = Color4 r g b a

-----------------------------------------
-- init window

setupWindow :: IO ()
setupWindow = do
  getArgsAndInitialize  
  initialDisplayMode $= [DoubleBuffered]    
  createWindow ""
  multisample $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  glSize (100, 100)

glSize :: (Float, Float) -> IO ()
glSize p@(w, h) = do
  windowSize $= fromPoint p
  projection2 0 w h 0
  where
    fromPoint (x, y) = Size (f x) (f y)
    f = toEnum . floor

projection2 xl xu yl yu = do
  matrixMode $= Projection
  loadIdentity
  ortho (f2d xl) (f2d xu) (f2d yl) (f2d yu) zl zu 
  matrixMode $= Modelview 0
  where
    zl = -5
    zu = 5

--------------------------------------------

getWindowSize :: IO (Int, Int)
getWindowSize = do
  Size w h <- G.get windowSize 
  return (fromEnum w, fromEnum h)

--------------------------------------------
-- converters

f2d = float2Double     

v2 (x, y) = vertex $ Vertex3 (f2d x) (f2d y) 0
p2v (x, y)= Vector3 (f2d x) (f2d y) 0


