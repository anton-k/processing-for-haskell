import Graphics.Proc

main = runProc $ def 
	{ procSetup  = setup
	, procDraw   = draw }

width = 400
height = 400

setup = do
	size (width, height)	
	noStroke
	forM [(x, y) | x <- [0, 5 .. width], y <- [0, 7 .. height]] $ \(x, y) -> do
		z <- noise2 (x / 100, y / 100)
		return ((x, y), 255 * z)
		
draw ps = do
	forM_ ps $ \(p, col) -> do
		fill (grey col)
		circle 5 p
		