import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw
	, procUpdate = update }

width = 400
height = 400
center = (0.5 * width, 0.5 * height)

setup = do
	size (width, height)
	background (grey 0)	
	stroke (grey 255)
	strokeWeight 2
	frameRate 30
	return (center, center)

draw (p1, p2) = do
	line p1 p2

len = 5

update (_, p) = do
	x <- random2 (-len, len)
	y <- random2 (-len, len)
	return (p, p + (x, y))
	