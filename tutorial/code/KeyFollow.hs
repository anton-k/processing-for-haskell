import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procKeyPressed = keyPressed }

width  = 400
height = 400
center = (0.5 * width, 0.5 * height)

setup = do
	size (width, height)
	return center

draw p = do
	background (grey 12)	
	fill (grey 255)	
	circle 15 p

dt = 5

keyPressed (x, y) = do 
	k <- key
	return $ case k of
		Char 'w'  -> (x, y - dt)
		Char 'a'  -> (x - dt, y)
		Char 's'  -> (x, y + dt)
		Char 'd'  -> (x + dt, y)

		SpecialKey KeyUp    -> (x, y - dt)
		SpecialKey KeyLeft  -> (x - dt, y)
		SpecialKey KeyDown  -> (x, y + dt)
		SpecialKey KeyRight -> (x + dt, y)
		_  -> (x, y)
