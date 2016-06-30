-- original code: https://processing.org/examples/keyboardfunctions.html
--
-- Keyboard Functions. Modified from code by Martin. Original 'Color Typewriter' concept by John Maeda.
-- 
-- Click on the window to give it focus and press the letter keys to type colors. 
-- The keyboard function keyPressed() is called whenever a key is pressed. keyReleased() 
-- is another keyboard function that is called when a key is released.
import Data.Char
import qualified Data.Map as M

import Graphics.Proc hiding (scale)

main = runProc $ def { procSetup = setup, procKeyPressed = keyPressed }

type St = ((Int, Int), M.Map Char Col)
 
width  = 640
height = 360

setup = do
  size (width, height)
  noStroke
  background (grey 101)
  -- todo
  -- colorMode(HSB, numChars)
  colorMap <- initColorMap
  return ((0, 0), colorMap)

initColorMap = fmap M.fromList $ forM ['a' .. 'z'] $ \ch -> fmap (\col -> (ch, col)) randomCol 

nx = 20
ny = 15

dx = width / float nx
dy = height / float ny

keyPressed :: St -> Pio St
keyPressed (p, colorMap) = do
	fill =<< fmap (getCol colorMap) key
	drawRect p
	return (nextCell p, colorMap)

getCol colorMap k = case k of
	Char ch -> maybe black id $ M.lookup (toLower ch) colorMap
	_ 		-> black

drawRect (x, y) = rect (float x * dx, float y * dy) (dx, dy)

nextCell (x, y) = (x1, y1)
	where
		x1 = (x + 1) `mod` nx
		y1 = if abs (x1 - x) /= 1 then (y + 1) `mod` ny else y

--------------------------------------
-- Side note
--
-- This example demonstrates how not only Haskell can borrow from Processing,
-- but how Processing can use advanced features of Haskell
--
-- We use associative map from letters to colors. No need to encode all collections with arrays.



