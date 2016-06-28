-- original code: https://processing.org/examples/datatypeconversion.html

-- Datatype Conversion.
-- 
-- It is sometimes beneficial to convert a value from one type of 
-- data to another. Each of the conversion functions converts its 
-- parameter to an equivalent representation within its datatype. 
-- The conversion functions include int(), float(), char(), byte(), and others.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

c = 'A';
f = float (fromEnum c)    -- Sets f = 65.0
i = int (f * 1.4)         -- Sets i to 91
-- b = byte(c / 2);       -- Sets b to 32

setup = do    
  size (width, height)
  noStroke
  font <- loadFont "FreeSans.ttf"
  textFont font 24

msg varName value dy = text ("The value of variable " ++ varName ++ " is " ++ show(value)) (50, 100 + dy)

draw () = do
  msg "c" c 0
  msg "f" f 50
  msg "i" i 100
  -- msg "b" b 150
