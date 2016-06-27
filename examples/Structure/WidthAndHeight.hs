-- the original example:
--
-- https://processing.org/examples/widthheight.html
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do
  size (width, height)

draw () = do
  background (grey 127)
  noStroke

  -- The forM_ is not a special construct of the language. 
  -- It's a library function reexported with the module `Control.Monad`.
  -- the forM_ is the same as mapM_ but the order of arguments is reversed.
  forM_ [0, 20 .. height] $ \i -> do
  	fill (rgb 129 206 15)
  	rect (0, i) (width, 10)
  	fill (grey 255)
  	rect (i, 0) (10, height)

---------------------------------------------
-- Sidenotes
--
-- notice the usage of width and height constants.
-- They have the different meaning from the values width and height
-- in processing. 
--
-- Haskell names for processing width and height are winWidth and winHeight.
-- But it's not so convenient to use as in Processing due to implicit sideeffect.
-- Haskell makes that side effect explicit. So we should write
--
-- > do
-- >   w <- winWidth
-- >   h <- winHeight
--
-- But we can work it around with a simple trick. We just define constants width and height
-- and then use them to set the sizes of the window:
--
-- > setup = do	
-- >   size (width, height)
--
-- Then we can use them just like in processing. Because they are pure constant values.
