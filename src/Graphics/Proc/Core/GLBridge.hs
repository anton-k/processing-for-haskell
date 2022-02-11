module Graphics.Proc.Core.GLBridge(
  MouseButton(..), Modifiers(..), Key(..), KeyState(..), Position(..), SpecialKey(..),
  -- Font,
  Col(..), glCol,
  d2f, f2d, v3,
  toVertex, toVector,
  glSize, setupWindow, getWindowSize
) where

import Graphics.Rendering.OpenGL hiding (scale, translate, rotate, rect, height, width)
import qualified Graphics.Rendering.OpenGL as G
import Graphics.UI.GLUT hiding (scale, translate, rotate, rect, rgba, Font)
-- import Graphics.Rendering.FTGL
import Graphics.Proc.Core.Vector
import Data.Default
import Control.Monad.IO.Class
import GHC.Float

-- | Color datatype. It contains values for three components of the color and transparency.
-- All values range in the interval from 0 to 1.
data Col = Col Float Float Float Float
    deriving (Show)

instance Default Col where
  def = black
    where black = Col 0 0 0 1

glCol :: Col -> Color4 Float
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
  glSize (P2 100 100)
  clearColor $= Color4 1 1 1 1
  G.clear [ColorBuffer]

glSize :: P2 -> IO ()
glSize p@(P2 w h) = do
  windowSize $= fromPoint p
  projection2 0 w h 0
  where
    fromPoint (P2 x y) = Size (f x) (f y)
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
d2f = double2Float

v3 :: IsPoint p => p -> IO ()
v3 = vertex . toVertex . toP3

toVector :: P3 -> Vector3 Double
toVector (P3 x y z) = Vector3 (f2d x) (f2d y) (f2d z)

toVertex :: P3 -> Vertex3 Double
toVertex (P3 x y z) = Vertex3 (f2d x) (f2d y) (f2d z)

