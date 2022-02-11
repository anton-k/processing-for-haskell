-- original code: https://processing.org/examples/iteration.html

-- Iteration.
--
-- Iteration with a "for" structure to construct repetitive forms.

import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

num = 14

setup = do
  size (P2 width height)
  stroke (greya 255 160)

draw () = do
	background (grey 102)
	noStroke

	-- White bars
	fill (grey 255)
	forM_ (ys (floor $ float num / 3) 60) $ \y -> do
		rect (P2 50 y) (P2 475 10)

 	-- Grey bars
 	fill (grey 51)
	forM_ (ys num 40) $ \y -> do
		rect (P2 405 y) (P2 30 10)

	forM_ (ys num 50) $ \y -> do
		rect (P2 425 y) (P2 30 10)

	-- Thin lines
	fill (grey 0)
	forM_ (ys (num - 1) 45) $ \y -> do
		rect (P2 120 y) (P2 40 1)

ys n start = take (n + 1) $ fmap (+ start) [0, 20 ..]

-----------------------------------------
-- Side note
--
-- In Haskell we don't have the for-loop construct.
-- But it can be emulated with function `forM_` (see docs for Control.Monad).
--
-- forM_ :: Monad m => [a] -> (a -> m ()) -> m ()
--
-- It takes a list of values and applies a procedure to all of them.
-- It's not so general as the for-loop, but there are it own advantages.
-- Notice how we can construct the values for looping programmatically.
--
