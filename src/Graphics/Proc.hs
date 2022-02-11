-- | Imperative EDSL for graphics and animation. The libary implements a Processing in Haskell.
--
-- An example:
--
-- > import Graphics.Proc
-- >
-- > main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }
-- >
-- > setup = do
-- >  size (300, 300)
-- >  return 0
-- >
-- > draw x = do
-- >  background (grey 255)
-- >  fill (rgb 0 255 0)
-- >  circle 20 (150 + 50 * sin x, 150)
-- >
-- > update x = return (x + 0.1)
-- >
-- We can find the quickstart guide and lots of examples in the project repository on github <https://github.com/anton-k/processing-for-haskell> (see the directory @examples@).
module Graphics.Proc(
  -- * Structure
  Proc(..), runProc,

  -- * Types
  Pio, Draw, Update, TimeInterval, Col(..), P2(..), P3(..), IsPoint(..),

  -- * Environment
  winSize, winWidth, winHeight,
  size,
  smooth, noSmooth, frameCount, frameRate,
  loop, noLoop, redraw,

  -- * Data
  -- | We can use ordinary Haskell datatypes primitive and composite ones.

  -- ** Conversion
  int, float,

  -- ** String Functions
  -- | We can use standard Haskell string functions.

  -- ** Array Functions
  --  | We can use Haskell arrays.

  -- * Control
  -- | We can use plain old Bool datatype.


  -- * Shape

  -- ** 2D Primitives

  triangle, rect, quad, ellipse, circle, line, linePath, point, pointPath, polygon,

  -- ** Curves
  bezier,

  -- ** 3D Primitives
  SphereRes(..), sphereDetail, sphere, box,

  -- ** Attributes
  EllipseMode, RectMode, DrawMode(..), ellipseMode, rectMode,
  strokeWeight,

  -- ** Vertex

  -- ** Loading & Displaying

  -- * Input

  -- ** Mouse
  mouse, mouseX, mouseY,
  relMouse, relMouseX, relMouseY,
  MouseButton(..),
  mouseButton,

  -- ** Keyboard
  Key(..), SpecialKey(..), key, Modifiers(..), modifiers,

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
  camera, camera2,

  -- ** Coordinates

    -- ** Material Properties

  -- * Color
  fill, noFill, stroke, noStroke, strokeFill,
  rgb, rgba, grey, greya, setAlpha,
  hsv, hsva,
  background, clear,

  white, black, navy, blue, aqua, teal, olive, green,
  lime, yellow, orange, red, maroon, fushsia, purple,
  gray, silver,


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
  randomSeed, random, random2, randomP2, randomCol, randomCola,
  randomGaussian,

  -- *** Perlin noise
  -- | Returns the Perlin noise value at specified coordinates. Perlin noise is a random sequence generator producing a more natural, harmonic succession of numbers than that of the standard random() function. It was developed by Ken Perlin in the 1980s and has been used in graphical applications to generate procedural textures, shapes, terrains, and other seemingly organic forms.
    --
    -- processing docs: <https://processing.org/reference/noise_.html>
  NoiseDetail(..), noiseDetail, noiseOctaves, noiseSeed, noise1, noise2, noise3,

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
  module Control.Monad.IO.Class,
  module Control.Applicative
) where

import Data.Default
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import Data.VectorSpace hiding (Sum(..))
import Data.NumInstances
import Data.AffineSpace
import Data.Cross

import Graphics.Proc.Core
import Graphics.Proc.Lib

