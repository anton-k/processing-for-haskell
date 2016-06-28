-- the original example: https://processing.org/examples/variables.html

-- Variables.
-- 
-- Variables are used for storing values. In this example, change the values of variables to affect the composition.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do	
	-- Sets the screen to be 640 pixels wide and 360 pixels high
	size (width, height)

draw () = do
    background (grey 0)
    stroke (grey 153)

    strokeWeight 4
    -- not implemented yet
    -- strokeCap(SQUARE);

    let a = 50
        b = 120
        c = 180

    line (a, b) (a+c, b)
    line (a, b+10) (a+c, b+10)
    line (a, b+20) (a+c, b+20)
    line (a, b+30) (a+c, b+30)

    let a1 = a + c
        b1 = height-b

    line (a1, b1) (a1+c, b1)
    line (a1, b1+10) (a1+c, b1+10)
    line (a1, b1+20) (a1+c, b1+20)
    line (a1, b1+30) (a1+c, b1+30)

    let a2 = a1 + c
        b2 = height-b1

    line (a2, b2)    (a2+c, b2)
    line (a2, b2+10) (a2+c, b2+10)
    line (a2, b2+20) (a2+c, b2+20)
    line (a2, b2+30) (a2+c, b2+30)

--------------------------------------------------------------
-- Sidenote
--
-- In the original example we update the mutable variables `a` and `b`.
-- But in Haskell there are no mutable variables. Local variables a pure constants
-- and we can not change them after assignment. That's why we have to give new 
-- names to variables a1, b1 and a2, b2.



-----------------------------------------------------------------------
--  More than one way to define variables
-----------------------------------------------------------------------
--
-- We could write this example in another style. We could define the local variables 
-- with `where` keyword:
--

draw2 () = do
    background (grey 0)
    stroke (grey 153)

    strokeWeight 4
    -- not implemented yet
    -- strokeCap(SQUARE);

    line (a, b) (a+c, b)
    line (a, b+10) (a+c, b+10)
    line (a, b+20) (a+c, b+20)
    line (a, b+30) (a+c, b+30)

    line (a1, b1) (a1+c, b1)
    line (a1, b1+10) (a1+c, b1+10)
    line (a1, b1+20) (a1+c, b1+20)
    line (a1, b1+30) (a1+c, b1+30)

    line (a2, b2)    (a2+c, b2)
    line (a2, b2+10) (a2+c, b2+10)
    line (a2, b2+20) (a2+c, b2+20)
    line (a2, b2+30) (a2+c, b2+30)
    where
        a = 50
        b = 120
        c = 180

        a1 = a + c
        b1 = height-b

        a2 = a1 + c
        b2 = height-b1

-- Also we could just write out the constants at the top level.