module Graphics.Proc.Lib.Environment(
	winSize, winWidth, winHeight,
	size,
	smooth, noSmooth, 
    frameCount, frameRate,
    redraw, loop, noLoop
) where

import qualified Control.Monad.Trans.State.Strict as S
import Graphics.Rendering.OpenGL

import Graphics.Proc.Core

-- | Return the pair of width and height as a 2D vector.
winSize :: Pio P2
winSize = liftA2 (,) winWidth winHeight

-- | System variable that stores the width of the display window. 
-- This value is set by the first parameter of the @size()@ function. 
-- For example, the function call @size(320, 240)@ sets the width 
-- variable to the value 320. The value of width defaults to 100 
-- if @size()@ is not used in a program.
--
-- processing docs: <https://processing.org/reference/width.html>
winWidth :: Pio Float
winWidth  = liftIO $ fmap (fromIntegral . fst) getWindowSize

-- | System variable that stores the height of the display window. 
-- This value is set by the second parameter of the @winSize()@ function. 
-- For example, the function call @winSize(320, 240)@ sets the height 
-- variable to the value 240. The value of height defaults to 100 
-- if @winSize()@ is not used in a program. 
--
-- processing docs: <https://processing.org/reference/height.html>
winHeight :: Pio Float
winHeight = liftIO $ fmap (fromIntegral . snd) getWindowSize

--------------------------------------------

-- | Defines the dimension of the display window width and height 
-- in units of pixels. In a program that has the setup() function, 
-- the size() function must be the first line of code inside setup().
--
-- processing docs: <https://processing.org/reference/size_.html>
size :: P2 -> Draw
size = liftIO . glSize

--------------------------------------------

-- | Draws all geometry with smooth (anti-aliased) edges. This behavior is 
-- the default, so @smooth()@ only needs to be used when a program needs to set 
-- the smoothing in a different way. The level parameter increases the level 
-- of smoothness. This is the level of over sampling applied to the graphics buffer.
--
-- processing docs: <https://processing.org/reference/smooth_.html>
smooth :: Draw
smooth = liftIO $ pointSmooth $= Enabled

-- | Draws all geometry and fonts with jagged (aliased) edges and images 
-- when hard edges between the pixels when enlarged rather than interpoloating pixels. 
-- Note that smooth() is active by default, so it is necessary to call noSmooth() 
-- to disable smoothing of geometry, fonts, and images. 
--
-- processing docs: <https://processing.org/reference/noSmooth_.html>
noSmooth :: Pio ()
noSmooth = liftIO $ pointSmooth $= Disabled

-- | Specifies the number of frames to be displayed every second. 
-- For example, the function call frameRate(30) will attempt to 
-- refresh 30 times a second. If the processor is not fast enough to
-- maintain the specified rate, the frame rate will not be achieved. 
-- Setting the frame rate within setup() is recommended. 
-- The default rate is 60 frames per second. 
--
-- processing docs: <https://processing.org/reference/frameRate_.html>
frameRate :: Float -> Pio ()
frameRate = putFrameRate


-- | Executes the code within draw() one time. This functions allows the program 
-- to update the display window only when necessary, for example when an event 
-- registered by mousePressed() or keyPressed() occurs.
--
-- In structuring a program, it only makes sense to call redraw() within events 
-- such as mousePressed(). This is because redraw() does not run draw() immediately 
-- (it only sets a flag that indicates an update is needed). 
--
-- processing docs: <https://processing.org/reference/redraw_.html>
redraw :: Draw
redraw = putLoopMode Redraw

-- | By default, Processing loops through draw() continuously, executing the code 
-- within it. However, the draw() loop may be stopped by calling noLoop(). 
-- In that case, the draw() loop can be resumed with loop().
-- 
-- processing docs: <https://processing.org/reference/loop_.html>
loop :: Draw
loop = putLoopMode Loop

-- | Stops Processing from continuously executing the code within draw(). 
-- If loop() is called, the code in draw() begins to run continuously again. 
-- If using noLoop() in setup(), it should be the last line inside the block.
--
-- processing docs: <https://processing.org/reference/noLoop_.html>
noLoop :: Draw
noLoop = putLoopMode NoLoop
