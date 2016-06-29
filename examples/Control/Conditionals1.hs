-- Conditionals 1.
-- 
-- Conditions are like questions. They allow a program to decide to take one 
-- action if the answer to a question is "true" or to do 
-- another action if the answer to the question is "false."
--
-- The questions asked within a program are always logical or 
-- relational statements. For example, if the variable 'i' is equal 
-- to zero then draw a line.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do    
  size (width, height) 

draw ()  = do
	background (grey 0)

	forM_ [10, 20 .. width] $ \i -> do		
		if (int i `mod` 20 == 0)
			then do
				stroke (grey 255)
				line (i, 80) (i, height / 2)
			else do
				stroke (grey 153)
				line (i, 20) (i, 180)


----------------------------------
-- Side note
--
-- We could easily transform imperative if-statment to pure one.
-- We can create a procedure that takes in color and the length 
-- of the line and pass values that are based on condition.
--

draw2 ()  = do
	background (grey 0)

	forM_ [10, 20 .. width] $ \i -> do		
		drawLine i $ 
			if (int i `mod` 20 == 0) 
				then (grey 255, 80, height / 2)
				else (grey 153, 20, 180)
	where
		drawLine i (col, y1, y2) = do
			stroke col
			line (i, y1) (i, y2)

