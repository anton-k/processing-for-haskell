-- Keyboard.
-- 
-- Click on the image to give it focus and press the letter 
-- keys to create forms in time and space. Each key has a 
-- unique identifying number. These numbers can be used to position shapes in space.
import Data.Char

import Graphics.Proc hiding (scale)

main = runProc $ def { procSetup = setup, procKeyPressed = keyPressed }
 
width  = 640
height = 360

setup = do
  size (width, height)
  noStroke
  background (grey 0)

rectWidth = width/4
  
keyPressed () = do
  k <- key
  case k of
    Char ch | isLetter ch-> do
      m <- millis
      let x = remap (0, 25) (0, width - rectWidth) (float $ (fromEnum ch - fromEnum 'a') `mod` 26)
      fill (grey $ float $ m `mod` 255)      
      rect (x, 0) (rectWidth, height)

    _       -> background (grey 0)
