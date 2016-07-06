-- original code: https://processing.org/examples/truefalse.html

-- True/False.
--  
-- A Boolean variable has only two possible values: true or false. 
-- It is common to use Booleans with control statements to determine 
-- the flow of a program. In this example, when the boolean value "x" is true, 
-- vertical black lines are drawn and when the boolean value "x" is false, 
-- horizontal gray lines are drawn.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

d = 20
middle = width / 2

setup = do    
  size (width, height)
  background (grey 0)
  stroke (grey 255)  

draw () = do
    forM_ [d, 2 * d .. width] $ \i -> do
        -- creates a boolean val
        let b = i < middle
        if (b) 
            then 
                -- Vertical line
                line (i, d) (i, height-d)
            else
                -- Horizontal line
                line (middle, i - middle + d) (width-d, i - middle + d)
