-- original code: https://processing.org/examples/charactersstrings.html

-- Characters Strings.

-- The character datatype, abbreviated as char, stores letters and 
-- symbols in the Unicode format, a coding system developed to support 
-- a variety of world languages. Characters are distinguished from 
-- other symbols by putting them between single quotes ('P').

-- A string is a sequence of characters. A string is noted by surrounding 
-- a group of letters with double quotes ("Processing"). Chars and strings 
-- are most often used with the keyboard methods, to display text to the screen, 
-- and to load images or files.

-- The String datatype must be capitalized because it is a complex datatype. 
-- A String is actually a class with its own methods, some of which are featured below.

{-# LANGUAGE OverloadedStrings #-}
import Graphics.Proc

import Prelude hiding (words)
import qualified Data.Text.Encoding as T
import Graphics.Rendering.FTGL as FTGL
import Codec.Binary.UTF8.String
import Data.String hiding  (words)
import Data.ByteString.Char8 hiding  (words, length)

main = runProc $ def { procSetup = setup, procDraw = draw, procKeyReleased = keyPressed }

width  = 640
height = 360

letter = "a"
wordsInit = "Begin..."

setup = do    
  size (width, height)
  font <- loadFont ("D:\\tools\\ubuntu-font-family\\Ubuntu-R.ttf")
  textFont font 36
  return wordsInit

draw words = do
  background (grey 0)

  textSize 14
  text "Click on the program, then type to add to the String" (50, 50)

  text ("Current key: " ++ letter) (50, 70)
  text ("The String is " ++ show(length words) ++  " characters long") (50, 90)
  
  textSize 28
  text (unpack $ T.encodeUtf8 $ fromString words) (50, 120) -- , 540, 300);

keyPressed words = do
  k <- key
  return $ case k of
    Char ch -> words ++ [ch]
    _       -> words

{-
void keyTyped() {
  // The variable "key" always contains the value 
  // of the most recent key pressed.
  if ((key >= 'A' && key <= 'z') || key == ' ') {
    letter = key;
    words = words + key;
    // Write the letter to the console
    println(key);
  }
}
-}