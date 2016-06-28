-- Integers Floats.
--
-- Integers and floats are two different kinds of numerical data. 
-- An integer (more commonly called an int) is a number without a decimal point. 
-- A float is a floating-point number, which means it is a number that has a decimal place. 
-- Floats are used when more precision is needed.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw, procUpdate = update }

width  = 640
height = 360

-- Creates a pair of values an integer and float.
setup :: Pio (Int, Float)
setup = do  
  -- Sets the screen to be 640 pixels wide and 360 pixels high
  size (width, height)
  stroke (grey 255)
  return (0, 0.0)

draw (a, b) = do
  background (grey 0)
  line (float a, 0) (float a, height/2)
  line (b, height/2) (b, height)

update (a, b) = return (a1, b1)
  where
    a1 = if (a > int width) then 0 else a + 1
    b1 = if (b > width) then 0 else b + 0.2

--------------------------------------------------------
-- Side note
--
-- Notice the need for explicit conversion with functions `float` and `int`. 
-- Processing can automatically convert ints to floats but in Haskell we
-- should convert the values manually
