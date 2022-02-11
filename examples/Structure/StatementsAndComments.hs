-- the original example:
--
-- https://processing.org/examples/statementscomments.html
--
-- Note that in processing if we want to produce static picture
-- we can write statments at the top-level. But in haskell we have to use callbacks.
-- We initialize window in the setup function and draw the picture at the draw function.
-- If we want it to be static we can just draw the same picture over and over again.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

-- The size function is a statement that tells the computer
-- how large to make the window.
-- Each function statement has zero or more parameters.
-- Parameters are data passed into the function
-- and are used as values for telling the computer what to do.
--
-- the state  type is unit or ().
setup = do
	size (P2 640 360)

-- The background function is a statement that tells the computer
-- which color (or gray value) to make the background of the display window
--
-- we make grey colors with function grey and rgb colors with rgb.
draw () = do
	background (rgb 204 153 0)
